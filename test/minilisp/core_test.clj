(ns minilisp.core-test
  (:require [clojure.test :refer :all]
            [minilisp.core :refer :all])
  (:refer-clojure :exclude [eval apply fn?]))

(deftest test-self-evaluating?
  (testing "numbers"
    (is (= 3 (value-of (eval-sexp '3 {}))))))

(deftest test-variable?
  (is (= 2 (value-of (eval-sexp 'a {'a 2})))))

(deftest test-def?
  (is (= (second (eval-sexp '(def a 3) {}))
         {'a 3})))

(deftest test-apply-promative
  (is (= 2 (value-of (eval-sexp '(+ 1 1)))))
  (is (= 5 (value-of (eval-sexp '(+ (+ 1 2) 1 1))))))

(deftest test-eval-program
  (is (= (eval-program '[3]) 3))
  (is (= (eval-program
          '[(def a 3)
            a])
         3)))

(deftest test-fn
  (is (= (value-of (eval-sexp '((fn [x] x) 2)))
         2))
  (is (= (eval-program '[(def f (fn [y] y))
                         (f 4)])
         4))
  (is (= (eval-program '[(def square (fn [x] (* x x)))
                         (+ (square 3) (square 4))])
         25)))
