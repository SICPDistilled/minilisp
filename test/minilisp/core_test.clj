(ns minilisp.core-test
  (:require
            [minilisp.core :refer :all]
            [expectations :refer :all])
  (:refer-clojure :exclude [eval true? apply]))

(expect '3
        (eval '3))

(expect 'TRUE
        (eval 'TRUE))

(expect 2
        (eval 'a {'a 2}))

(expect 2
        (eval '(+ 1 1)))

(expect 5
        (eval '(+ (+ 1 2) 1 1)))

(expect {'a 3}
        (:env (eval-sexp '(def a 3) {})))

(expect '2
        (eval '(if TRUE 2 3)))

(expect '3
        (eval '(if FALSE 2 3)))

(expect '(if test
           result)
        (cond->if '(cond test result)))

(expect '(if c1
           r1
           (if c2
             r2))
        (cond->if '(cond c1
                        r1
                        c2
                        r2)))

(expect 3
        (eval '(if TRUE 3)))

(expect '(if TRUE 3)
        (cond->if '(cond TRUE
                     3
                     )))

(expect 3
        (eval '(cond TRUE 3)))

(expect 3
        (eval-program '[3]))

(expect 3
        (eval-program
         '[(def a 3)
           a]))

(expect 2
        (eval '((fn [x] x) 2)))

(expect 4
        (eval-program '[(def f (fn [y] y))
                        (f 4)]))

(expect 25
        (eval-program '[(def square (fn [x] (* x x)))
                        (+ (square 3) (square 4))]))

(expect 1
        (eval-program '[(if TRUE 1 2)]))

(expect 2
        (eval-program '[(if FALSE 1 2)]))

(expect 'NIL
        (eval '(if FALSE 1)))

(expect 1
        (eval-program '[(def a 1)
                        (if TRUE 1)
                        a]))

(expect 8
        (eval-program '[(def add-n (fn [n]
                                     (fn [r]
                                       (+ n r))))
                        ((add-n 3) 5)]))

(expect '((fn [a c]
             (+ a c))
          b d)
        (let->fn '(let [a b
                        c d]
                    (+ a c))))

(expect 3
        (eval '(let [a 3]
                a)))

(expect 5
        (eval '(let [f (fn [x] (+ x 2))]
                 (f 3))))

(expect 'TRUE
        (eval-program '[(def cons (fn [x y]
                                    (fn [m]
                                      (cond (= m 0) x
                                            (= m 1) y))))

                        (def car (fn [z]
                                   (z 0)))

                        (def cdr (fn  [z]
                                   (z 1)))

                        (def t (cons 3 4))
                        (and (= (car t) 3)
                             (= (cdr t) 4))

                        ]))
