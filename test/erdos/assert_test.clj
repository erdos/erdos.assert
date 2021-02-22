(ns erdos.assert-test
  (:require [clojure.test :refer [deftest testing is]]
            [erdos.assert :as ea]))

;; imports all private vars with "-" prefix for testing
(let [target (the-ns 'erdos.assert)]
  (doseq [[k v] (ns-map target)
          :when (and (var? v) (= target (.ns ^clojure.lang.Var v)))
          :when (:private (meta v))]
    (eval `(defn ~(symbol (str "-" k)) [~'& args#]
             (apply (deref ~v) args#)))))

(defmacro is-print [expression & lines]
  (let [expected (str (clojure.string/join \newline lines) \newline)]
    `(is (= ~expected (second (ea/examine-str ~expression))))))

(deftest test-verify
  (is (nil? (ea/verify (= 1 1))))
  (is (thrown? clojure.lang.ExceptionInfo (ea/verify (= 1 2)))))

(deftest test-assert
  (is (nil? (ea/assert (= 1 1)))
  (is (thrown? AssertionError (ea/assert (= 1 2))))))

(deftest test-lazy?
  (testing "Delays"
    (is (-lazy? (delay 1)))
    (is (not (-lazy? (doto (delay 1) deref)))))
  (is (not (-lazy? (range 100))))
  (is (not (-lazy? [1 2 3])))
  (is (-lazy? (map inc (range 10))))
  (is (not (-lazy? (list '1 2 3))))
  (is (not (-lazy? (seq {1 2 3 4}))))
  (is (not (-lazy? (seq [1 2 3])))))

(deftest test-java-method-accessor
  (is (nil? (-java-method-accessor 123)))
  (is (nil? (-java-method-accessor '(print ""))))
  (is (nil? (-java-method-accessor '(.. x length))))
  (is (nil? (-java-method-accessor '(. a b c))))
  (is (= 'length (-java-method-accessor '(.length "")))))

(deftest test-print-line-impl
  (letfn [(tester [x] (with-out-str (-print-line-impl print print x)))]
    (testing "Special cases"
      (is (= "'(+ 1 2 3)" (tester '(quote (+ 1 2 3))))))
    (testing "Raw types"
      (is (= "asdf" (tester 'asdf)))
      (is (= "\"asdf\"" (tester "asdf")))
      (is (= ":x" (tester :x)))
      (is (= ":x/y" (tester :x/y)))
      (is (= "123M" (tester 123M)))
      (is (= "12.2" (tester 12.2)))
      (is (= "1/3" (tester 1/3)))
      (is (= "nil" (tester nil)))
      (is (= "true" (tester true))))
    (testing "Not lazy lists."
      (is (= "(1 2 3 4)" (tester '(1 2 3 4))))
      (is (= "(1 2)" (tester (seq [1 2]))))
      (is (= "(1)" (tester '(1))))
      (is (= "()" (tester ()))))
    (testing "Dereferencing values"
      (is (= "@abc" (tester '(clojure.core/deref abc)))))
    (testing "Vectors."
      (is (= "[1 2 3]" (tester (vector 1 2 3))))
      (is (= "[]" (tester (vector)) (tester []))))
    (testing "Maps."
      (is (= "{}" (tester {})))
      (is (= "{}" (tester (new java.util.HashMap))))
      (is (= "{1 2, 3 4}" (tester (hash-map 1 2 3 4)))))
    (testing "Lazy lists."
      (is (= "(0 …)" (tester (range))))
      (is (= "(1 2)" (tester (seq [1 2]))))
      (is (= "([1 2])" (tester (seq {1 2}))))
      (is (= "(0 1 2 …)" (tester (doto (range) (->> (take 3) (dorun))))))
      (is (= "(…)" (tester (take 10 (range)))))
      (is (= "(…)" (tester (lazy-seq nil))))
      (is (= "(1 2 3)" (tester (list 1 2 3))))
      (is (= "(1 2 …)" (tester (list* 1 2 (lazy-seq nil)))))
      (is (= "(1 2 0 …)" (tester (list* 1 2 (range)))))
      (is (= "(0 1 2 3 4 5 6 7 8 9)" (tester (range 10)))))))

;; TODO: test for printing quoted form!

(deftest test-examine-1
  (testing "Let forms"
    )
  (testing "Function forms")
  (testing "Macros"
    (is-print ,(-> 4 (inc) (* 2) (dec))
              "(-> 4 (inc) (* 2) (dec))"
              "¦     ¦     ¦"
              "9     5     10 ")
    (is-print ,(-> 4 inc (* 2) (dec))
              "(-> 4 inc (* 2) (dec))"
              "¦     ¦   ¦"
              "9     5   10 ")
    (testing "Second branch of macro is not printed"
      (ea/examine-str (and (* 1 2) (+ 1 2)))))
  (testing "Loop expression with explicit recur"
    (is-print ,(loop [i 4] (when (pos? i) (println :> i) (recur (dec i))))
              "(loop [i 4] (when (pos? i) (println :> i) (recur (dec i))))"
              "¦                 ¦        ¦                     ¦"
              "nil               ¦        nil, nil, nil, nil    3, 2, 1, 0 "
              "                  true, true, true, true, false "))
  (testing "Multiple values"
    (is-print ,(dotimes [i 4] (println (* i i)))
              "(dotimes [i 4] (println (* i i)))"
              "¦              ¦        ¦"
              "nil            ¦        0, 1, 4, 9 "
              "               nil, nil, nil, nil "))
  (testing "Java reflection"
    (is-print ,(. "hello" substring 2)
              "(. \"hello\" substring 2)"
              "           ¦"
              "           \"llo\" ")
    (is-print ,(. "hello" (substring 2))
              "(. \"hello\" (substring 2))"
              "           ¦"
              "           \"llo\" ")
    (is-print ,(.substring "hello" 2)
              "(.substring \"hello\" 2)"
              "¦"
              "\"llo\" "))
  (testing "Function call"
    (is-print () "()")
    (is-print ,(vector (+ 1 2) 3 4)
              "(vector (+ 1 2) 3 4)"
              "¦       ¦"
              "[3 3 4] 3 ")))

(deftest test-is
  (ea/is (= 2 (inc 1))))

(deftest test-are
  (ea/are [x y] (= (inc x) y)
     1 2
     2 3
     4 5))

(deftest test-show-meta
  (testing "Add ^:show metadata to let binding"
    (is-print (doseq [^:show i (range 3)])
              "(doseq [i (range 3)])"
              "¦       ¦ ¦"
              "nil     ¦ (0 1 2) "
              "        0, 1, 2 "))
  (testing "Add ^:show metadata to function parameter"
    (is-print (mapv (fn [^:show a] (inc a)) [1 2 3])
              "(mapv (fn [a] (inc a)) [1 2 3])"
              "¦          ¦  ¦"
              "[2 3 4]    ¦  2, 3, 4 "
              "           1, 2, 3 ")
    (is-print (mapv (fn* [^:show a] (inc a)) [1 2 3])
              "(mapv (fn* [a] (inc a)) [1 2 3])"
              "¦           ¦  ¦"
              "[2 3 4]     ¦  2, 3, 4 "
              "            1, 2, 3 ")
    (is-print (mapv (fn* ([^:show a] (inc a))) [1 2 3])
              "(mapv (fn* ([a] (inc a))) [1 2 3])"
              "¦            ¦  ¦"
              "[2 3 4]      ¦  2, 3, 4 "
              "             1, 2, 3 ")))

(comment ;; run these test cases manually

  (examine (* (+ 19 17) (- 19 17)))

  (examine (= (:a {:a (str "asd")}) (or (+ 1 2) (+ 3 4) (+ 5 6)) (+ 2 (- 4 3 (* 2 3 4)))))

  (examine (let [a (+ 1 2) b (* 3 4)] (* (+ a b) (- a b))))

  (examine (map (fn [a] (+ a 1)) [1 2 3 4]))

  (examine (doall (for [i [1 2 3 4]] (inc i))))

  (examine (doseq [i [1 2 3 4]] (print (* i i)) (print (* 2 i))))

  (-macroexpand-code '(doseq [i [1 2 3]] XXXX YYYY))

  ;; here the value of 'a is also printed
  (examine (mapv (fn [^:show a] (+ a 1)) [1 2 3 4]))
)