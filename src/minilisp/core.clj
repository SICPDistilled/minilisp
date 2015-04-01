(ns minilisp.core
  (:refer-clojure :exclude [fn? eval true?]
                  :rename {apply clj-apply}))

(declare apply primitive-procedure eval)

(defn map-vals [f m]
  (into (empty m) (for [[k v] m]
                    [k (f v)])))

(defn error [& msg] (assert false (clj-apply str msg)))

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [exp]
  (or (number? exp)
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
    (if (nil? alternative)
      'NIL
      (eval alternative env))))

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

(defn let->fn [[_ bindings body]]
  (let [bindings-map (clj-apply hash-map bindings)
        names (keys bindings-map)
        values (vals bindings-map)]
    (cons
     (list 'fn
           names
           body)
     values)))

(defn and->if [[pred & rest]]
  (if (nil? rest)
    (list 'if pred 'TRUE)
    (list 'if
          pred
          (and->if rest))))

(defn eval-sexp [sexp env]
  (cond
   (self-evaluating? sexp)
   [sexp env]

   (primitive-procedure sexp)
   [(primitive-procedure sexp) env]

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

      (= op 'let)
      ;; (let [[bindings body] operands]
      ;;   (eval-sexp body (merge env
      ;;                          (map-vals #(eval % env) (clj-apply hash-map bindings)))))
      (eval-sexp (let->fn sexp) env)

      (= op 'and)
      (eval-sexp (and->if operands) env)

      (= op 'fn)
      (let [[params body] operands]
        [(make-procedure params
                         body
                         env)
         env])

      :else
      [(apply (eval op env)
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

(def primitive-procedure {'+ + '- - '* * '/ / '= =})

(def primitive-procedure? (set (vals primitive-procedure)))

(def compound-procedure? map?)

(defn apply [proc args]
  (cond
   (primitive-procedure? proc)
   (clj-apply proc args)

   (compound-procedure? proc)
   (eval (:body proc)
         (merge
          (:env proc)
          (zipmap (:params proc)
                  args)))

   :else
   (error "APPLY FAIL: " proc args)))
