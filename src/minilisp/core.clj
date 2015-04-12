(ns minilisp.core
  (:refer-clojure :exclude [eval true?]
                  :rename {apply clj-apply}))

(declare apply primitive-procedure eval primitive-procedure-name? primitive-procedure-map)

(defn map-vals [f m]
  (into (empty m) (for [[k v] m]
                    [k (f v)])))

(defn error [& msg] (assert false (clj-apply str msg)))

(def bools #{'TRUE 'FALSE})

(defn self-evaluating? [sexp]
  (or (number? sexp)
      (bools sexp)))

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

(defn cond->if [[_ & pairs]]
  (pairs->if pairs))

(defn let->fn [[_ bindings body]]
  (let [params (take-nth 2 bindings)
        args (take-nth 2 (rest bindings))]
    (cons
     (make-fn params body)
     args)))

(defn and->if [[pred & rest]]
  (if (nil? rest)
    (make-if pred 'TRUE)
    (make-if pred
             (and->if rest))))

(defrecord State [result env])

(defrecord Proc [params body env])

(defn eval-sexp [sexp env]
  (cond
   (self-evaluating? sexp)  ; If it is self evaluating
   (State. sexp env)        ; return it and dont change env

   (primitive-procedure-name? sexp)              ; if it's a primative procedure
   (State. (primitive-procedure-map sexp) env)   ; look it up in primitive-procedure-map

   (symbol? sexp)           ; If it is a symbol
   (State. (env sexp) env)  ; Look it up in env, env unchanged

   (seq? sexp)                 ; Otherwise, it's a sequence
   (let [[op & operands] sexp] ; We destructure the operator and operands
     (cond
      (= op 'def)                           ; If it's a def
      (State. 'NIL                          ; Return nil and
              (let [[name exp] operands     ; Fetch out the name and expression
                    value (eval exp env)]   ; evaluate the expression
                (assoc env name value)))    ; and assoc the name in the env to the value

      (= op 'if)                            ; If it's an if
      (State. (eval-if sexp env) env)       ; evaluate it using a special rule and don't change the env

      (= op 'cond)
      (eval-sexp (cond->if sexp) env)

      (= op 'let)
      (eval-sexp (let->fn sexp) env)

      (= op 'and)
      (eval-sexp (and->if operands) env)

      (= op 'fn)                          ; If it's a fn
      (let [[params body] operands]       ; destructure the params and body from operands
        (State. (Proc. params body env)   ; Return a Proc of the parameters and body that closes over the current env
                env))                     ; Without changing it

      :else                                     ; Otherwise
      (State. (apply (eval op env)              ; We assume it's a function call and apply the evaluated operator
                     (map (fn [operand]         ;   (which may be a primitive a Proc)
                            (eval operand env)) ; to the evaluated operands
                          operands))
              env)))                            ; again, without changing the environment

   :else
   (error "EVAL FAIL: " sexp)))

(defn eval
  ([sexp] (eval sexp {}))
  ([sexp env]
     (:result (eval-sexp sexp env))))

(defn next-state [last-state sexp]
  (let [env (:env last-state)]
    (eval-sexp sexp env)))

(def initial-state (State. 'NIL {}))

(defn eval-program [sexps]
  (:result (reduce next-state initial-state sexps)))

(def primitive-procedure-map {'+ + '- - '* * '/ /
                              '= (fn [& args]
                                   (if (clj-apply = args) 'TRUE 'FALSE))
                              'square (fn [x] (* x x))})

(def primitive-procedure-name? (set (keys primitive-procedure-map)))
(def primitive-procedure? (set (vals primitive-procedure-map)))

(def compound-procedure? map?)

(defn apply [proc args]
  (cond
   (primitive-procedure? proc)   ; if it's a primitive procedure
   (clj-apply proc args)         ; apply it (in Clojure) to the args

   (compound-procedure? proc)    ; if it's a compound procedure
   (eval (:body proc)            ; evaluate the body
         (merge                  ; in a new environment made
          (:env proc)            ; by taking the environment closed over on creation
          (zipmap (:params proc) ; and assigning the formal parameters to arguments
                  args)))

   :else
   (error "APPLY FAIL: " proc args)))
