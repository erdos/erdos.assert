(ns erdos.assert
  "A small library for power assertions support in Clojure."
  (:refer-clojure :exclude [assert]))


(defn- quoted? [x] (and (seq? x) ('#{quote clojure.core/quote} (first x))))


(defn- walk
  "Like clojure.walk/walk but preserves meta."
  [inner outer form]
  (cond
    (list? form) (outer (with-meta (apply list (map inner form)) (meta form)))
    (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
    (seq? form) (outer (doall (with-meta (map inner form) (meta form))))

    (set? form) (outer (with-meta (set (map inner form)) (meta form)))
    (map? form) (outer (with-meta (into (empty form) (map inner form)) (meta form)))
    (vector? form) (outer (with-meta (mapv inner form) (meta form)))

    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (coll? form) (outer (into (empty form) (map inner form)))
    :else (outer form)))


(defn- prewalk [f form] (walk (partial prewalk f) identity (f form)))


(defn- postwalk-code
  "Like clojure.walk/postwalk but preserves meta and does not walk quoted forms."
  [f expr]
  (if (quoted? expr)
    expr
    (walk (partial postwalk-code f) f expr)))


(defn print-bars [x->vals]
  (let [x->str (into {} (for [[x vs] x->vals]
                          [x (clojure.string/join ", " (map pr-str vs))]))
        x->m (into (sorted-map) (for [[x s] x->str] [x {:str s}]))
        xs   (reverse (keys x->m))
        rf   (fn [m x]
               (let [width (inc (count (x->str x)))
                     x-max (+ x width)
                     h     (apply max 0 (map (comp :height val) (subseq m > x < x-max)))]
                 (assoc m x {:height (inc h)
                             :string (x->str x)
                             :width  width})))
        heights   (reduce rf (sorted-map) xs)
        draw-line (fn [heights]
                    (->
                     (reduce (fn [{:keys [x' result]}
                                  [x {:keys [width height string]}]]
                               (dotimes [t (- x x')] (print " "))
                               (if (pos? height)
                                 (do (print "¦")
                                     {:x'     (+ x 1)
                                      :result (assoc result x {:width width
                                                               :height (dec height)
                                                               :string string})})
                                 (do
                                   (print string) (print " ")
                                   {:x' (+ x width)
                                    :result result})))
                             {:x' 0, :result (sorted-map)} heights)
                     (doto (do (println))) ;; print a new line after evaluated.
                     :result
                     ))]
    (dorun (take-while seq (iterate draw-line heights)))))


(defn- macroexpand-code [form]
  (prewalk (fn [x] (if (quoted? x) x
                       (let [e (macroexpand x)]
                         (if (instance? clojure.lang.IObj e)
                           (with-meta e (meta x))
                           e)))) form))


(defmulti ^:private pass-add-loggers (fn [e] (when (seq? e) (first e))))


(defmethod pass-add-loggers 'quote [expr] expr)


(defmethod pass-add-loggers nil [e]
  (cond (seq? e)
        (if-let [m-key (-> e meta ::key)]
          (list 'log m-key (map pass-add-loggers e))
          e)

        (vector? e) (mapv pass-add-loggers e)

        (set? e) (set (map pass-add-loggers e))

        (map? e) (zipmap (map pass-add-loggers (keys e))
                         (map pass-add-loggers (vals e)))

        :default e))


(defmethod pass-add-loggers :default [e]
  (if-let [m-key (-> e meta ::key)]
    (list 'log m-key (map pass-add-loggers e))
    (map pass-add-loggers e)))

;; TODO: handle fn* too

(defmethod pass-add-loggers 'let* [[_ bindings & bodies]]
  `(let* [~@(mapcat vec (for [[k v] (partition 2 bindings)]
                          [k (pass-add-loggers v)]
                          ))]
     ~@(map pass-add-loggers bodies)))

(defmethod pass-add-loggers 'do [[_ & bodies]]
  `(do ~@(map pass-add-loggers bodies)))

(defmethod pass-add-loggers 'letfn* [[_ bind & bodies]]
  `(letfn* ~bind ~@(map pass-add-loggers bodies)))


(defn- print-line
  "Prints an expression on a single line.
   Calculates positions of annotated objects in string."
  [expr]
  (let [pad  (atom 0)
        out  (new java.lang.StringBuilder)
        bars (atom {})
        strout   (fn [x]
                   (.append out (str x))
                   (swap! pad + (count x)))
        space    (partial strout " ")
        print    (comp strout pr-str)]
    ((fn act [expr]
       (when-let [m-key (-> expr meta ::key)]
         (swap! bars assoc @pad m-key))
       (cond
         (or (seq? expr) (list? expr))
         (do (strout "(")
             (print (first expr)) ;; we do not eval all fn calls
             (doseq [y (next expr)]
               (space) (act y))
             (strout ")"))

         (vector? expr) ;; the same
         (do (strout "[")
             (act (first expr))
             (doseq [x (next expr)]
               (space) (act x))
             (strout "]"))

         (set? expr) ;; the same
         (do (strout "#{")
             (act (first expr))
             (doseq [x (next expr)]
               (space) (act x))
             (strout "}"))

         (map? expr) ;; the same
         (do (strout "{")
             (act (key (first expr))) (space) (act (val (first expr)))
             (doseq [x (next expr)]
               (strout ", ") (act (val x)) (space) (act (key x)))
             (strout "}"))

         :default
         (print expr)))
     expr)
    {:out (str out), :bars @bars}))


(defn- pass-add-keys [expr]
  (let [id         (atom 0)
        gen-id     (partial swap! id inc)
        f (fn [e]  (cond (quoted? e) e
                         ;; (instance? clojure.lang.IObj e) (with-meta e (assoc (meta e)) ::key (gen-id))

                         (or (list? e) (seq? e) (symbol? e))
                         (with-meta e (assoc (meta e) ::key (gen-id)))

                         :default e))]
    (postwalk-code f expr)))


(defmacro emit-code [expr]
  (let [keyed-expr (pass-add-keys expr)
        keyed-expanded-expr (macroexpand-code keyed-expr)
        logged-exprs (pass-add-loggers keyed-expanded-expr)
        line-to-print (print-line keyed-expr)]
    `(let [state# (atom {})
           ~'log  (fn [i# val#] (swap! state# update i# conj val#) val#)
           e#     (delay ~logged-exprs)]
       {:result e#
        :state  (delay @e# @state#)
        :print  (delay
                 @e#
                 (with-out-str
                   (println ~(:out line-to-print))
                   (print-bars
                    (into {} (for [[k# v#] ~(:bars line-to-print)
                                   :when (contains? @state# v#)]
                               [k# (get @state# v#)])))))})))


(defmacro assert
  ([e] (assert e ""))
  ([e msg]
   (when *assert*
     `(let [code# (emit-code ~e)]
        (when-not @(:result code#)
          (throw (new AssertionError (str ~msg \newline @(:print code#)))))))))


(defmacro examine
  "Prints expression to output. Returns value of expression."
  [expr]
  `(let [code# (emit-code ~expr)
         result# @(:result code#)]
     (println)
     (println @(:print code#))
     result#))

'good
