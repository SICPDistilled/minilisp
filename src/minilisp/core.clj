(ns minilisp.core
  (:refer-clojure :exclude [fn? eval true?]
                  :rename {apply clj-apply}))

(declare apply primitive-procedure eval primitive-procedure-name? primitive-procedure-map)

(defn map-vals [f m]
  (into (empty m) (for [[k v] m]
                    [k (f v)])))

(defn error [& msg] (assert false (clj-apply str msg)))

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [exp]
  (or (number? exp)
      (bools exp)))

(defn true? [sexp]
  (not= 'FALSE sexp))

(defn eval-if [[_ pred consequent alternative] env]
  (if (true? (eval pred env))
    (eval consequent env)
    (if (nil? alternative)
      'NIL
      (eval alternative env))))

(defn eval-let [[bindings body] env]
  (eval body (merge env
                    (map-vals #(eval % env) (clj-apply hash-map bindings)))))

(defn make-if
  ([pred consequence]
     (list 'if pred consequence))
  ([pred consequence alternative]
     (list 'if pred consequence alternative)))

(defn make-fn [params body]
  (list 'fn params body))

(defn pairs->if [[pred consequence & pairs]]
  (if (nil? pairs)
    (make-if pred consequence)
    (make-if pred
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
     (make-fn names body)
     values)))

(defn and->if [[pred & rest]]
  (if (nil? rest)
    (make-if pred 'TRUE)
    (make-if pred
             (and->if rest))))

(defrecord State [result env])

(defrecord Proc [params body env])

(defn eval-sexp [sexp env]
  (cond
   (self-evaluating? sexp)
   (State. sexp env)

   (primitive-procedure-name? sexp)
   (State. (primitive-procedure-map sexp) env)

   (symbol? sexp)
   (State. (env sexp) env)

   (seq? sexp)
   (let [[op & operands] sexp]
     (cond
      (= op 'def)
      (State. 'NIL
              (let [[name exp] operands
                    value (eval exp env)]
                (assoc env name value)))

      (= op 'if)
      (State. (eval-if sexp env) env)

      (= op 'cond)
      (eval-sexp (cond->if sexp) env)

      (= op 'let)
      (eval-sexp (let->fn sexp) env)
      ;; (eval-let operands env)

      (= op 'and)
      (eval-sexp (and->if operands) env)

      (= op 'fn)
      (let [[params body] operands]
        (State. (Proc. params body env)
                env))

      :else
      (State. (apply (eval op env)
                     (map (fn [operand]
                            (eval operand env))
                          operands))
              env)))

   :else
   (error "EVAL FAIL: " sexp)))

(defn eval
  ([sexp] (eval sexp {}))
  ([sexp env]
     (:result (eval-sexp sexp env))))

(defn eval-program [sexps]
  (:result (reduce (fn [{:keys [env]} sexp]
                      (eval-sexp sexp env))
                    {:env {}}
                    sexps)))

(def primitive-procedure-map { '+ + '- - '* * '/ / '= =
                               'square (fn [x] (* x x))})

(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))

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
