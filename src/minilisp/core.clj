(ns minilisp.core
  (:refer-clojure :exclude [fn? eval true?]))

(declare apply-proc primitive-procedure eval)

(defn error [& msg] (assert false (apply str msg)))

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [exp]
  (or (number? exp)
      (primitive-procedure exp)
      (bools exp)))

(def get-env second)

(def value-of first)

(defn make-procedure [params body env]
  {:params params
   :body body
   :env env})

(defn true? [sexp]
  (not= 'FALSE sexp))

(defn eval-if [[_ pred consequent alternative] env]
  (if (true? (eval pred env))
    (eval consequent env)
    (eval alternative env)))

(defn pairs->if [[pred consequence & pairs]]
  (if (nil? pairs)
    (list 'if pred consequence)
    (list 'if
          pred
          consequence
          (pairs->if pairs))))

(defn cond->if [sexp]
  (let [pairs (rest sexp)]
    (pairs->if pairs)))

(defn eval-sexp [sexp env]
  (cond
   (self-evaluating? sexp)
   [sexp env]

   (symbol? sexp)
   [(env sexp) env]

   (seq? sexp)
   (let [[op & operands] sexp]
     (cond
      (= op 'def)
      [nil
       (let [[name exp] operands
             value (eval exp env)]
         (assoc env name value))]

      (= op 'if)
      [(eval-if sexp env)
       env]

      (= op 'cond)
      (eval-sexp (cond->if sexp) env)

      (= op 'fn)
      (let [[params body] operands]
        [(make-procedure params
                         body
                         env)
         env])

      :else
      [(apply-proc (eval op env)
                   (map (fn [operand]
                          (eval operand env))
                        operands))
       env]))

   :else
   (error "EVAL FAIL: " sexp)))

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

(defn apply-proc [proc args]
  (cond
   (primitive-procedure proc)
   (apply (primitive-procedure proc) args)

   (compound-procedure? proc)
   (eval (:body proc)
         (merge
          (:env proc)
          (zipmap (:params proc)
                  args)))

   :else
   (error "apply-proc error")))
