(ns erdos.assert
  "A small library for power assertions support in Clojure."
  (:refer-clojure :exclude [assert])
  (:require [clojure.test :as test]
            [clojure.string :as s]))

;; Algorithm consists of the following passes.
;;
;; Compile time:
;; 1. Walk the EDN expression and assign a distinct value to the ::key key in the metadata map of every node.
;; 2. Execute a special macroexpansion on the expression that preserves metadata maps.
;; 3. Walk the expanded form and wrap every function call in a logging function.
;; 4. Print the original expression into a string and calculate the positions of every sub-expression identified by the metadata map value under ::key.
;;
;; Runtime:
;; 1. On evaluation, every logging function call will put the evaluation result into a local state atom.
;; 2. When evaluated to false, the state atom (R/1) and the positions (C/4) are used to print the diagram to the standard output.
;;

(def ^:private ellipsis "…")
(def ^:private pipe "¦") ; also check "║" and "│"

(defn- show-meta [x] (:show (meta x)))
(defn- with-show-meta [x] (vary-meta x assoc :show true))
(defn- get-meta-key [x] (::key (meta x)))

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

    (set? form) (outer (with-meta (into (empty form) (map inner form)) (meta form)))
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
  (clojure.core/assert (sorted? heights))
  (->
   (fn [[x' result]
         x {:keys [width height string]}]
      (dotimes [t (- x x')]
        (print " "))
      (if (pos? height)
        (do (print pipe)
            [(inc x)
             (assoc result x {:width width
                              :height (dec height)
                              :string string})])
        (do (print string)
            (print " ")
            [(+ x width) result])))
   (reduce-kv [0 (sorted-map)] heights)
   (second)
   (doto (do (println))))) ;; print a new line after evaluated.


(defn- java-method-accessor [x]
  (when (and (seq? x)
             (symbol? (first x))
             (not= '.. (first x))
             (not= '. (first x))
             (.startsWith (name (first x)) "."))
    (symbol (.substring (name (first x)) 1))))

;; TODO: (examine (.substring (.substring "hello" (inc 1)) 1))
;; too mmuch offset here.

(defn- macroexpand-code
  "Like clojure.walk/macroexpand-all but preserves meta and does not expand quoted forms."
  [form]
  (prewalk
   (fn [x]
     (if (quoted? x)
       x
       (if-let [accessor-sym (java-method-accessor x)]
         (let [[member target & args] x]
           (list* '. target (with-meta accessor-sym (meta member)) (map macroexpand-code args)))
         (let [e (macroexpand x)]
           (if (instance? clojure.lang.IObj e)
             (with-meta e (meta x))
             e)))))
   form))


;; name of the global logger method.
(def ^:private logger-sym '+assert-logger+)


(defmulti ^:private pass-add-loggers (fn [e] (when (seq? e) (first e))))


;; augments a clojure expression by adding logging to marked subexpressions.
(defmethod pass-add-loggers 'quote [expr] expr)


(defmethod pass-add-loggers nil [e]
  (cond ; (seq? e) - other implementations

        (and (symbol? e) (show-meta e))
        (list logger-sym (get-meta-key e) e)

        (vector? e) (mapv pass-add-loggers e)

        (set? e) (set (map pass-add-loggers e))

        (map? e) (zipmap (map pass-add-loggers (keys e))
                         (map pass-add-loggers (vals e)))

        :default e))


(defmethod pass-add-loggers :default [e]
  (if-let [m-key (get-meta-key e)]
    (list logger-sym m-key (map pass-add-loggers e))
    (map pass-add-loggers e)))


(defmethod pass-add-loggers '. [[_ target form & args]]
  (cond-> (list* '.
                (pass-add-loggers target)
                (if (seq? form)
                  (list (cons (first form) (map pass-add-loggers (next form))))
                  (list* form (map pass-add-loggers args))))
    (get-meta-key form) (->> (list logger-sym (get-meta-key form)))))


;; (examine ((fn* ([x] (inc x))) 3))
(defmethod pass-add-loggers 'fn* [[_ & clauses]]
  (if (every? seq? clauses)
    (list* 'fn*
           (for [[params & bodies] clauses
                 :let [param-shown (filter show-meta (tree-seq coll? seq (first clauses)))]]
             `(~params ~@(for [p param-shown] (list logger-sym (get-meta-key p) p))
                       ~@(map pass-add-loggers bodies))))
    (let [param-shown (filter show-meta (tree-seq coll? seq (first clauses)))]
      `(fn* ~(first clauses)
            ~@(for [p param-shown] (list logger-sym (get-meta-key p) p))
            ~@(map pass-add-loggers (next clauses))))))


(defmethod pass-add-loggers 'finally [[_ & bodies]]
  (list* 'finally (map pass-add-loggers bodies)))


(defmethod pass-add-loggers 'catch [[_ e-type e-name & bodies]]
  (list* 'catch e-type e-name (map pass-add-loggers bodies)))

;; not needed. standard macroexpansion handles this case as well.
;
; (defmethod pass-add-loggers 'do [[_ & bodies]]
;  `(do ~@(map pass-add-loggers bodies)))

(defmethod pass-add-loggers 'let* [[_ bindings & bodies]]
  `(let* [~@(mapcat vec (for [[k v] (partition 2 bindings)] 
                         [k (cond->> (pass-add-loggers v)
                              (show-meta k) (list logger-sym (get-meta-key k)))]))]
     ~@(map pass-add-loggers bodies)))


(defmethod pass-add-loggers 'letfn* [[_ bindings & bodies]]
  `(letfn* [~@(mapcat vec (for [[k v] (partition 2 bindings)] [k (pass-add-loggers v)]))]
    ~@(map pass-add-loggers bodies)))


(defn- print-line-prep
  "Creates a buffer and a writer function. Returns a triple of:
   - A function that prints to a local buffer.
   - A function returning the current horizontal offset.
   - A function returning the string value of the print buffer."
  []
  (let [out  (new java.lang.StringBuilder)]
    [(fn strout [^String s] (.append out s))
     (fn length [] (.length out))
     (fn string [] (str out))]))


(defn- lazy? [x] (and (instance? clojure.lang.IPending x) (not (realized? x))))


(defn- rest*
  "Like clojure.core/rest but returns nil when form is already realized and tail is nil."
  [x]
  (as->
      (cond
        (instance? clojure.lang.Cons x) (.more ^clojure.lang.Cons x)
        :otherwise (rest x))
      * (if (lazy? *) * (seq *))))


(defn- split-with-rest
  "Like clojure.core/split-with but eagerly produces the first part of the result tuple
   and uses the rest function from the first argument."
  [rest pred coll]
  (loop [xs coll
         consumed []]
    (if-let [[x] (seq xs)]
      (if (pred x)
        (recur (rest xs) (conj consumed x))
        [(seq consumed) xs])
      [coll nil])))


(defmulti ^:private print-line-impl (fn [print-string print-child-fn expr] (type expr)))


(defmethod print-line-impl :default [strout _ expr] (strout (pr-str expr)))


(defmethod print-line-impl java.lang.Throwable [strout _ expr]
  (strout "↯ ")
  (strout (.getName (class expr))))


;; Prints lists in the usual edn format. Lazy parts of the lists are printed with ellipsis.
(defmethod print-line-impl java.util.List [strout print expr]
  (cond
  
    (and (list? expr) (quoted? expr))
    (do ;; if quoted form
      (strout "'")
      (print (second expr)))

    (and (list? expr) (= 'clojure.core/deref (first expr)))
    (do (strout "@")
        (print (second expr)))

    :else
    (do
      (strout "(")
      (if (lazy? expr)
        (strout ellipsis)
        (let [tails   (take-while some? (iterate rest* expr))
              [as bs] (split-with-rest rest* #(and (some? %) (not (lazy? %))) tails)]
          (when (seq (first as))
            (print (ffirst as)))

          (doseq [a (next as) ;; itt megallunk.
                  :while (seq a)]
            (strout " ")
            (print (first a)))

          (when bs
            (when (seq as) (strout " "))
            (strout ellipsis))))
      (strout ")"))))


(defmethod print-line-impl clojure.lang.IPersistentVector [strout print expr]
  (strout "[")
  (when (seq expr)
    (print (first expr))
    (doseq [x (next expr)]
      (strout " ") (print x)))
  (strout "]"))


(prefer-method print-line-impl clojure.lang.IPersistentVector java.util.List)


(defmethod print-line-impl java.util.Set [strout print expr]
  (strout "#{")
  (when (seq expr)
    (print (first expr))
    (doseq [x (next expr)]
      (strout " ") (print x)))
  (strout "}"))


(defmethod print-line-impl java.util.Map [strout act expr]
  (strout "{")
  (when (seq expr)
    (act (key (first expr)))
    (strout " ")
    (act (val (first expr)))
    (doseq [x (next expr)]
      (strout ", ") (act (key x)) (strout " ") (act (val x))))
  (strout "}"))

(defmethod print-line-impl clojure.lang.AFunction [strout _ expr]
  (strout "λ ")
  (strout (.getName (class expr))))


(defn- print-line
  "Prints an expression on a single line.
   Calculates positions of annotated objects in string.
   Returns a pair of:
   - string of the print result
   - a map of horizontal offset to ::key identifier"
  [expr]
  (let [[strout get-pad get-val] (print-line-prep)
        bars (volatile! {})]
    ((fn act [expr]
       (when-let [m-key (get-meta-key expr)]
         (vswap! bars assoc (get-pad) m-key))
       (print-line-impl strout act expr))
     expr)
    [(get-val) @bars]))


(defn- pass-add-keys
  "Adds a random ::key metadata to all subexpressions."
  [expr]
  (postwalk-code
   (fn [e]
     (cond (quoted? e) e
           (= () e) e
           (or (list? e)
               (seq? e)
               (symbol? e)) (vary-meta e assoc ::key (list 'quote (gensym "powerassert")))
           :default e))
   expr))


(defn -safe-pr-str [obj] (first (print-line obj)))


(defn- print-bars [x->vals]
  (clojure.core/assert (map? x->vals))
  (let [x->str (into {} (for [[x vs] x->vals] [x (s/join ", " vs)]))
        rf   (fn [m x]
               (let [width (inc (count (x->str x)))
                     x-max (+ x width)
                     h     (apply max 0 (map (comp :height val) (subseq m > x < x-max)))]
                 (assoc m x {:height (inc h)
                             :string (x->str x)
                             :width  width})))]
    (->> (keys x->vals)
         (sort >)
         (reduce rf (sorted-map))
         (iterate draw-line)
         (take-while not-empty)
         (dorun))))


(defn -emit-code-print [value-delay print-result-str bars-map state-atom]
  (delay
    (deref value-delay)
    (with-out-str
      (println print-result-str)
      (print-bars (into {} (for [[k# v#] bars-map
                                 :when (contains? @state-atom v#)]
                              [k# (get @state-atom v#)]))))))


(defn- emit-code [expr]
  (let [keyed-expr          (pass-add-keys expr)
        [out bars]          (print-line keyed-expr)
        keyed-expanded-expr (macroexpand-code keyed-expr)
        logged-exprs        (pass-add-loggers keyed-expanded-expr)]
    `(let [state# (atom {})
           ~logger-sym  (fn [i# val#]
                    ;; we stringify here because values may change during evaluation
                    (swap! state# update i# (fnil conj []) (-safe-pr-str val#))
                    val#)
           e#     (delay ~logged-exprs)]
        [e# (-emit-code-print e# ~out ~bars state#)])))


(defmacro assert
  "Power assert macro.
   Like clojure.core/assert but the thrown AssertionError message contains the expression examined."
  ([e] `(assert ~e ""))
  ([e msg]
   (when *assert*
     `(let [[result# print#] ~(emit-code e)]
        (when-not @result#
          (->> @print#
               (str ~(or (not-empty msg) "Assert failed:") \newline)
               (new AssertionError)
               (throw)))))))


(defmacro verify
  "Like assert but throws ExceptionInfo when condition does not hold and can not be turned off with *assert* var."
  ([e] `(verify ~e ""))
  ([e msg]
   `(let [[result# print#] ~(emit-code e)]
      (when-not @result#
        (throw (ex-info (str ~msg \newline @print#)
                        {:form (quote ~e) :print @print#}))))))


(defmacro examine-str
  "Returns tuple of evaluated value and examined string."
  [expr]
  `(let [[result# print#] ~(emit-code expr)]
     [@result# @print#]))


(defmacro examine
  "Prints expression to *out*. Returns value of expression."
  [expr]
  `(let [[result# printable#] (examine-str ~expr)]
     (println)
     (println printable#)
     result#))


(defmacro is
  "Drop-in replacement for the clojure.test/is macro."
  ([expr] `(is ~expr nil))
  ([expr msg]
   `(try
      (let [[result# print#] ~(emit-code expr)
            res#  @result#]
        (if res#
          (test/do-report {:type :pass, :message ~msg, :expected '~expr, :actual res#})
          (test/do-report {:type :fail, :message (str ~msg \newline @print#),
                           :expected '~expr, :actual res#})))
      (catch Throwable t#
        (test/do-report
         {:type :error, :message ~msg, :expected '~expr, :actual t#})))))


(defmacro are
  "Drop-in replacement for the clojure.test/are macro."
  [argv expr & args]
  (assert (vector? argv)
          "First argument should be a list of symbols.")
  (assert (zero? (rem (count args) (count argv)))
          "The number of parameters for each test case should match the number of elements in the first parameter.")
  (let [argv-symbols (set (filter symbol? (tree-seq coll? seq argv)))
        expr         (postwalk-code (fn [e] (if (argv-symbols e) (with-show-meta e) e)) expr)]
    (cons 'do
          (for [part (partition (count argv) args)]
            `(let [~argv ~(vec part)]
              (is ~expr))))))


'good
