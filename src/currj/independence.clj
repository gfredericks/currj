(ns currj.independence
  "Functions for pulling out independent sub-forms")

(def scalar? (some-fn number? true? false? string? keyword? nil?))

(defn- ground?
  [form]
  (or (scalar? form)
      (and (map? form)
           (every? ground? (mapcat identity form)))
      (and (vector? form)
           (every? ground? form))
      (and (set? form)
           (every? ground? form))))

(declare process)

(defn- deshadow
  "Given a (macroexpanded) form and a list of locals, returns a
  possibly different form that does not shadow any of the locals or
  any of its own locals."
  ([form locals] (deshadow form locals {}))
  ([form locals subs]
     (cond
       (symbol? form)
       (if (contains? subs form) (subs form) form)

       (ground? form)
       form

       (seq? form)
       (let [sf (first form)]
         (cond
           (= sf 'if)
           (cons 'if (for [form (rest form)]
                       (deshadow form locals subs)))

           ;; let* and fn* are the non-trivial ones

           (= sf 'let*)
           (let [[new-lettings new-locals new-subs]
                 (reduce (fn [[lettings locals subs] [name expr]]
                           (if (some #{name} locals)
                             (let [new-name (gensym (str name "-unshadowing-"))
                                   new-subs (assoc subs name new-name)]
                               [(conj lettings new-name (deshadow expr locals new-subs))
                                locals
                                new-subs])
                             [(conj lettings name (deshadow expr locals subs))
                              (conj locals name)
                              subs]))
                         [[] locals subs]
                         (partition 2 (second form)))]
             (list 'let* new-lettings (deshadow (last form) new-locals new-subs)))

           ;; TODO: this assumes only one arity defined
           (= sf 'fn*)
           (let [[_fn* args expr] form
                 shadowers (filter (set locals) args)
                 shadow-map (zipmap shadowers
                                    (for [name shadowers]
                                      (gensym (str name "-unshadowing-"))))]
             (list 'fn*
                   (vec (for [name args] (or (shadow-map name) name)))
                   (deshadow expr (into locals args) (merge subs shadow-map))))

           ;; assume fn-call
           :else
           (for [form form] (deshadow form locals subs))))

       (or (vector? form) (set? form))
       (into (empty form) (for [form form] (deshadow form locals subs)))

       (map? form)
       (into {} (for [[k v] form] [(deshadow k locals subs)
                                   (deshadow v locals subs)]))))
)

(defn- independent?
  [form unknown-locals]
  (true? (first (process form unknown-locals))))

(defn- process-if-then
  [[_if cond-form true-form false-form :as if-form] unknown-locals]
  (let [[c? t? f?] (map #(independent? % unknown-locals) (rest if-form))]
    (if c?
      (if (and t? f?)
        ;; If everything is independent we can just precompute the
        ;; entire form
        (let [g (gensym "if")]
          [[[g if-form]] g])

        ;; We precompute the cond and then conditionally do any
        ;; precomputations for the true-form and false-form
        (let [c (gensym "if-condition")
              [true-lettings new-true-form] (process true-form unknown-locals)
              [false-lettings new-false-form] (process false-form unknown-locals)]
          [(remove nil?
                   [[c cond-form]
                    (when-not (empty? true-lettings)
                      [(vec (map first true-lettings)) (list 'if c (vec (map second true-lettings)))])
                    (when-not (empty? false-lettings)
                      [(vec (map first false-lettings)) (list 'if-not c (vec (map second false-lettings)))])])
           (list 'if c new-true-form new-false-form)]))

      ;; If the cond is not independent then we can only precompute
      ;; stuff from the cond.
      (let [[lettings new-cond-form] (process cond-form unknown-locals)]
        [lettings (list 'if new-cond-form true-form false-form)]))))

(defn- process-if
  [if-form unknown-locals]
  ;; eh who cares
  (process-if-then (concat if-form [nil]) unknown-locals))

(defn- process-fn-call
  [[f-form & arg-forms :as fn-form] unknown-locals]
  (if (every? #(independent? % unknown-locals) fn-form)
    [true fn-form]
    (reduce
     (fn [[lettings forms] next-form]
       (if (independent? next-form unknown-locals)
         (if (or (ground? next-form) (symbol? next-form))
           [lettings (concat forms [next-form])]
           (let [g (gensym)]
             [(conj lettings [g next-form])
              (concat forms [g])]))
         (let [[lettings' form'] (process next-form unknown-locals)]
           [(into lettings lettings')
            (concat forms [form'])])))
     [[] ()]
     fn-form)))

(defn- process-let
  [let-form unknown-locals]
  (let [[_let bindings-form body] (deshadow let-form unknown-locals)]
    (if (empty? bindings-form)
      (process body unknown-locals)
      ;; We're going one pair at a time, recursively
      (let [[name expr & more-bindings] bindings-form]
        (if (independent? expr unknown-locals)
          ;; If the pair is independent, we can pull it out entirely
          (let [[lettings new-form] (process-let (list 'let*
                                                       (vec more-bindings)
                                                       body)
                                                 unknown-locals)]
            [(cons [name expr] lettings)
             new-form])
          ;; If the pair is not independent, we have to be sure to add
          ;; it to the list of unknown-locals
          (let [[expr-lettings new-expr] (process expr unknown-locals)]
            (if (empty? more-bindings)
              (let [[body-lettings new-body] (process body (conj unknown-locals name))]
                [(concat expr-lettings body-lettings)
                 (list 'let* [name new-expr] new-body)])
              ;; argh; there's no guarantee that this thing comes back as
              ;; a proper let given the empty? short-circuit above. I guess
              ;; it doesn't matter.
              (let [[let-lettings new-let] (process-let (list 'let*
                                                              (vec more-bindings)
                                                              body)
                                                        (conj unknown-locals name))]
                [(concat expr-lettings let-lettings)
                 (if (and (seq? new-let) (= 'let* (first new-let)))
                   (list 'let*
                         (vec (concat [name new-expr] (second new-let)))
                         (last new-let))
                   (list 'let* [name new-expr] new-let))]))))))))

(defn- process
  "If fully independent, returns [true form]. Otherwise returns
  [lettings new-form] where lettings being empty indicates that
  nothing can be precomputed."
  [form unknown-locals]
  (cond

   (symbol? form)
   [(if (some #{form} unknown-locals) [] true)
    form]

   (seq? form)
   (cond

    (= 'if (first form))
    (if (= 3 (count form))
      (process-if form unknown-locals)
      (process-if-then form unknown-locals))

    (= 'let* (first form))
    (process-let form unknown-locals)

    :else
    (process-fn-call form unknown-locals))

   (ground? form)
   [true form]))

(defn refactor
  "Given a form (with no macros left unexpanded) and a list
  of symbols whose values are not known yet, factors out all
  sub-expressions that are independent of the unknown-locals;
  returns [lettings new-form] where lettings is a list of pairs
  such as you might have in let, and the new-form is what you
  want it's dinner time."
  [form unknown-locals]
  (let [[lettings form] (process form unknown-locals)]
    [(if (true? lettings) [] lettings) form]))
