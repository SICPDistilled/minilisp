(ns minilisp.core
  (:refer-clojure :exclude [fn?]))

(declare apply-proc primative-procedure value-of)

(defn error [& msg] (apply println msg))

(defn self-evaluating? [exp]
  (or (number? exp)
      (primative-procedure exp)))

(defn variable? [exp]
  (symbol? exp))

(defn def? [exp]
  (and (seq? exp)
       (= (first exp) 'def)))

(defn third [col]
  (nth col 2))

(defn def-name [exp]
  (second exp))

(defn def-value [exp]
  (third exp))

(defn application? [sexp]
  (seq? sexp))

(defn operator [sexp]
  (first sexp))

(defn operands [sexp]
  (rest sexp))

(defn fn? [exp]
  (and (seq? exp)
       (= 'fn (first exp))))

(defn fn-params [exp]
  (second exp))

(defn fn-body [exp]
  (third exp))

(defn make-procedure [params body env]
  {:params params
   :body body
   :env env})

(defn eval-sexp
  ([sexp] (eval-sexp sexp {}))
  ([sexp env]
     (cond (self-evaluating? sexp)
           [sexp env]

           (variable? sexp)
           [(env sexp) env]

           (def? sexp)
           [nil (let [name (def-name sexp)
                      value (def-value sexp)]
                  (assoc env name (value-of (eval-sexp value env))))]

           (fn? sexp)
           [(make-procedure (fn-params sexp)
                            (fn-body sexp)
                            env)
            env]

            (application? sexp)
            [(apply-proc (value-of (eval-sexp (operator sexp) env))
                         (map (fn [operand]
                                (value-of (eval-sexp operand env)))
                              (operands sexp)))
             env]

            :else
            (error "EVAL FAIL" sexp))))

(defn eval-program [sexps]
  (value-of (reduce (fn [[return-value env] sexp]
                      (eval-sexp sexp env))
                    [nil {}]
                    sexps)))

(def value-of first)

(def primative-procedure {'+ + '- - '* * '/ /})

(def compound-procedure? map?)

(defn apply-primative-procedure [proc args]
  (apply (primative-procedure proc) args))

(defn apply-proc [proc args]
  (cond (primative-procedure proc)
        (apply-primative-procedure proc args)

        (compound-procedure? proc)
        (value-of ( eval-sexp (:body proc)
                              (merge
                               (:env proc)
                               (zipmap (:params proc)
                                       args))))

        :else
        (error "apply-proc error")))
