(ns erdos.assert-test
  (:require [clojure.test :refer :all]
            [erdos.assert :as ea]))

;; TODO: do the same to macros maybe?
(let [target (the-ns 'erdos.assert)]
  (doseq [[k v] (ns-map target)
          :when (and (var? v) (= target (.ns v)))]
    (eval `(defn ~(symbol (str "-" k)) [~'& args#]
             (apply (deref ~v) args#)))))

(deftest test-print-line-impl
  (letfn [(tester [x] (with-out-str (-print-line-impl print print x)))]
    (testing "Not lazy lists."
      (is (= "(1 2 3 4)" (tester '(1 2 3 4))))
      (is (= "(1 2)" (tester (seq [1 2])))))
    (testing "Vectors."
      (is (= "[1 2 3]" (tester (vector 1 2 3))))
      (is (= "[]" (tester (vector)))))
    (testing "Lazy lists."
      (is (= "(0 …)" (tester (range))))
      (is (= "(0 1 2 …)" (tester (doto (range) (->> (take 3) (dorun))))))
      (is (= "(…)" (tester (take 10 (range)))))
      (is (= "(…)" (tester (lazy-seq nil))))
      (is (= "(1 2 …)" (tester (list* 1 2 (lazy-seq nil)))))
      (is (= "(1 2 0 …)" (tester (list* 1 2 (range)))))
      (is (= "(0 1 2 3 4 5 6 7 8 9)" (tester (range 10)))))))

;; TODO: test for printing quoted form!

(deftest test-examine-1
  (testing "Let forms"
    )
  (testing "Function forms")
  (testing "Macros"
    (testing "Second branch of macro is not printed"
      (ea/examine-str (and (* 1 2) (+ 1 2)))))
  (testing "Multiple values"
    (ea/examine-str (dotimes [i 4] (println (* i i)))))
  (testing "Function call"
    (is (= [[3 3 4]
            "(vector (+ 1 2) 3 4)\n¦       ¦\n[3 3 4] 3 \n"]
           (ea/examine-str (vector (+ 1 2) 3 4))))))


                                        ; (ea/examine (* (+ 19 17) (- 19 17)))

;

; (ea

                                        ;(examine (= (:a {:a (str "asd")}) (or (+ 1 2) (+ 3 4) (+ 5 6)) (+ 2 (- 4 3 (* 2 3 4)))))

                                        ;(examine (let [a (+ 1 2) b (* 3 4)] (* (+ a b) (- a b))))

                                        ;(examine (map (fn [a] (+ a 1)) [1 2 3 4]))

                                        ;(examine (doall (for [i [1 2 3 4]] (inc i))))

                                        ;(examine (doseq [i [1 2 3 4]] (print (* i i)) (print (* 2 i))))

                                        ; (macroexpand-code '(doseq [i [1 2 3]] XXXX YYYY))
