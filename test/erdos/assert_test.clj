(ns erdos.assert-test
  (:require [clojure.test :refer :all]
            [erdos.assert :as ea]))

; (ea/examine (* (+ 19 17) (- 19 17)))


                                        ;(examine (= (:a {:a (str "asd")}) (or (+ 1 2) (+ 3 4) (+ 5 6)) (+ 2 (- 4 3 (* 2 3 4)))))

                                        ;(examine (let [a (+ 1 2) b (* 3 4)] (* (+ a b) (- a b))))

                                        ;(examine (map (fn [a] (+ a 1)) [1 2 3 4]))

                                        ;(examine (doall (for [i [1 2 3 4]] (inc i))))

                                        ;(examine (doseq [i [1 2 3 4]] (print (* i i)) (print (* 2 i))))

                                        ; (macroexpand-code '(doseq [i [1 2 3]] XXXX YYYY))
