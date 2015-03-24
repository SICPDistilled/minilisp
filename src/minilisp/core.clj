(ns minilisp.core
  (:refer-clojure :exclude [fn? eval true?]))

(declare apply-proc primitive-procedure eval)

(defn error [& msg] (apply println msg))

(defn third [col]
  (nth col 2))

;; Identifying expressions
(defn self-evaluating? [exp]
  (or (number? exp)
      (primitive-procedure exp)
      (= 'TRUE exp)
      (= 'FALSE exp)))

(defn starts-with? [sexp sym]
  (and (seq? sexp)
       (= (first sexp) sym)))

(defn variable? [exp]
  (symbol? exp))

(defn def? [exp]
  (starts-with? exp 'def))

(def get-env second)

(defn def-name [exp]
  (second exp))

(defn def-value [exp]
  (third exp))

;; Dealing with applications, ie (<operator> *<operands>)
(defn application? [sexp]
  (seq? sexp))

(defn operator [sexp]
  (first sexp))

(defn operands [sexp]
  (rest sexp))

;; Dealing with fns
(defn fn? [exp]
  (starts-with? exp 'fn))

(defn fn-params [exp]
  (second exp))

(defn fn-body [exp]
  (third exp))

;; Turn fns to an 'applyable' object
(defn make-procedure [params body env]
  {:params params
   :body body
   :env env})

(def value-of first)

(defn if? [sexp]
  (starts-with? sexp 'if))

(defn true? [sexp]
  (if (= 'FALSE sexp)
    false
    true))

(defn cond? [sexp]
  (starts-with? sexp 'cond))

(defn eval-if [[_ pred consequent alternative] env]
  (if (true? (eval pred env))
    (eval consequent env)
    (eval alternative env)))

(defn pairs->if [[pred consequence & pairs]]
  (if (nil? pairs)
    (list 'if pred consequence)
    (let [tail (pairs->if pairs)]
      (list 'if
            pred
            consequence
            tail))))

(defn cond->if [sexp]
  (let [pairs (rest sexp)]
    (pairs->if pairs)))

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

           (if? sexp)
           [(eval-if sexp env)
            nil]

           (cond? sexp)
           (eval-sexp (cond->if sexp) env)

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

(defn eval
  ([sexp] (eval sexp {}))
  ([sexp env]
     (value-of (eval-sexp sexp env))))

(defn eval-program [sexps]
  (value-of (reduce (fn [[return-value env] sexp]
                      (eval-sexp sexp env))
                    [nil {}]
                    sexps)))

(def primitive-procedure {'+ + '- - '* * '/ /})

(def compound-procedure? map?)

(defn apply-primitive-procedure [proc args]
  (apply (primitive-procedure proc) args))

(defn apply-proc [proc args]
  (cond
   (primitive-procedure proc)
   (apply-primitive-procedure proc args)

   (compound-procedure? proc)
   (eval (:body proc)
         (merge
          (:env proc)
          (zipmap (:params proc)
                  args)))

   :else
   (error "apply-proc error")))
