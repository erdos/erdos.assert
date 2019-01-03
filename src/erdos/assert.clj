(ns erdos.assert
  "A small library for power assertions support in Clojure."
  (:refer-clojure :exclude [assert])
  (:require [clojure.test :as test]
            [clojure.string :as s]))

(set! *warn-on-reflection* true)

(defn- quoted?
  "Decides if parameter is a quoted sexp form."
  [x] (boolean (and (seq? x) ('#{quote clojure.core/quote} (first x)))))

(defn- walk
  "Like clojure.walk/walk but preserves meta map."
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


(defn- prewalk
  "Like clojure.walk/prewalk but preserves meta map."
  [f form] (walk (partial prewalk f) identity (f form)))


(defn- postwalk-code
  "Like clojure.walk/postwalk but preserves meta and does not walk quoted forms."
  [f expr]
  (if (quoted? expr)
    expr
    (walk (partial postwalk-code f) f expr)))

(defn- draw-line [heights]
  (->
   (fn [{:keys [x' result]}
               [x {:keys [width height string]}]]
             (dotimes [t (- x x')] (print " "))
             (if (pos? height)
               (do (print "¦")
                   {:x'     (+ x 1)
                    :result (assoc result x {:width width
                                             :height (dec height)
                                             :string string})})
               (do (print string)
                   (print " ")
                   {:x' (+ x width)
                    :result result})))
   (reduce {:x' 0, :result (sorted-map)} heights)
   (doto (do (println))) ;; print a new line after evaluated.
   :result))

(defn- macroexpand-code [form]
  (prewalk
   (fn [x]
     (if (quoted? x)
       x
       (let [e (macroexpand x)]
         (if (instance? clojure.lang.IObj e)
           (with-meta e (meta x))
           e))))
   form))


(defmulti ^:private pass-add-loggers (fn [e] (when (seq? e) (first e))))


;; augments a clojure expression by adding logging to marked subexpressions.
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

(defn- print-line-prep
  "Returns a triple of:
   - a function that prints to a local buffer
   - an atom holding the current horizontal offset
   - a delay holding the string value of the print buffer"
  []
  (let [pad (atom 0)
        out  (new java.lang.StringBuilder)
        strout   (fn [x]
                   (.append out (str x))
                   (swap! pad + (count x)))]
    [strout
     pad
     (delay (str out))]))

(defn- lazy? [x] (and (instance? clojure.lang.IPending x) (not (realized? x))))

(def ^:private ellipsis "…")

(defn rest* [x]
  (as->
      (cond
        (instance? clojure.lang.Cons x) (.more ^clojure.lang.Cons x)
        :otherwise (rest x))
      * (if (lazy? *) * (seq *))))

(defn- split-with-rest [rest f xs1]
  (loop [xs xs1
         consumed []]
    (if-let [[x] (seq xs)]
      (if (f x)
        (recur (rest xs) (conj consumed x))
        [(seq consumed) xs])
      [xs1 nil])))

(defn- print-line-seq [strout print expr]
  (strout "(")
  (if (lazy? expr)
    (strout ellipsis)
    (let [tails   (take-while some? (iterate rest* expr))
          [as bs] (split-with-rest rest* #(and (some? %) (not (lazy? %))) tails)]
      (when (seq as)
        (print (ffirst as)))

      (doseq [a (next as) ;; itt megallunk.
              :while (seq a)]
        (strout " ")
        (print (first a)))

      (when bs
        (when (seq as) (strout " "))
        (strout ellipsis))))
  (strout ")"))

(defn- print-line
  "Prints an expression on a single line.
   Calculates positions of annotated objects in string."
  [expr]
  (let [[strout pad out-delay] (print-line-prep)
        bars     (atom {})
        space    (partial strout " ")]
    ((fn act [expr]
       (when-let [m-key (-> expr meta ::key)]
         (swap! bars assoc @pad m-key))
       (cond
         (or (seq? expr) (list? expr))
         (print-line-seq strout act expr)

         (vector? expr) ;; the same
         (do (strout "[")
             (when (seq expr)
               (act (first expr))
               (doseq [x (next expr)]
                 (space) (act x)))
             (strout "]"))

         (set? expr) ;; the same
         (do (strout "#{")
             (when (seq expr)
               (act (first expr))
               (doseq [x (next expr)]
                 (space) (act x)))
             (strout "}"))

         (map? expr) ;; the same
         (do (strout "{")
             (when (seq expr)
               (act (key (first expr))) (space) (act (val (first expr)))
               (doseq [x (next expr)]
                 (strout ", ") (act (val x)) (space) (act (key x))))
             (strout "}"))

         :default
         (print expr)))
     expr)
    {:out @out-delay, :bars @bars}))


(defn- pass-add-keys
  "Adds a random ::key metadata to all subexpressions."
  [expr]
  (postwalk-code
   (fn [e]
     (cond (quoted? e) e
           (or (list? e)
               (seq? e)
               (symbol? e)) (vary-meta e assoc ::key (list 'quote (gensym "powerassert")))
           :default e))
   expr))

(defn- safe-pr-str [obj] (:out (print-line obj)))

(defn print-bars [x->vals]
  (clojure.core/assert (map? x->vals))
  (let [x->str (into {} (for [[x vs] x->vals]
                          [x (s/join ", " (map safe-pr-str vs))]))
        rf   (fn [m x]
               (let [width (inc (count (x->str x)))
                     x-max (+ x width)
                     h     (apply max 0 (map (comp :height val) (subseq m > x < x-max)))]
                 (assoc m x {:height (inc h)
                             :string (x->str x)
                             :width  width})))]
    (->> (keys x->vals)
         (sort)
         (reverse)
         (reduce rf (sorted-map))
         (iterate draw-line)
         (take-while seq)
         (dorun))))

(defmacro -emit-code [expr]
  (let [keyed-expr (pass-add-keys expr)
        keyed-expanded-expr (macroexpand-code keyed-expr)
        logged-exprs (pass-add-loggers keyed-expanded-expr)
        line-to-print (print-line keyed-expr)]
    `(let [state# (atom {})
           ~'log  (fn [i# val#] (swap! state# update i# (fnil conj []) val#) val#)
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
  "Power assert macro.
   Like clojure.core/assert but the thrown AssertionError message contains the expression examined."
  ([e] (assert e ""))
  ([e msg]
   (when *assert*
     `(let [code# (-emit-code ~e)]
        (when-not @(:result code#)
          (throw (new AssertionError (str ~msg \newline @(:print code#)))))))))

(defmacro examine-str
  "Returns tuple of evaluated value and examined string."
  [expr]
  `(let [code# (-emit-code ~expr)]
     [@(:result code#)
      @(:print code#)]))

(defmacro examine
  "Prints expression to *out*. Returns value of expression."
  [expr]
  `(let [[result# printable#] (examine-str ~expr)]
     (println)
     (println printable#)
     result#))

(comment

  (examine (assoc {} :c (+ 1 2 3 4 (* 3 4 5))))

  comment)

;; TODO: test this.
#_
(defmacro is
  ([expr] (is expr nil))
  ([expr msg]
   `(try
      (let [code# (-emit-code ~expr)]
        (if-let [res# @(:result code#)]
          (test/do-report {:type :pass, :message ~msg, :expected '~expr, :actual res#})
          (test/do-report {:type :fail, :message (str ~msg \newline @(:print code#)),
                           :expected '~expr, :actual res#})))
      (catch Throwable t#
        (test/do-report
         {:type :error, :message ~msg, :expected '~expr, :actual t#})))))

'good
