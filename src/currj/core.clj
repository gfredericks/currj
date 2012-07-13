(ns currj.core
  (:require [clojure.walk :as wk]
            [clojure.set :as sets]))

(defn- unqualified-symbol? [x] (and (symbol? x) (nil? (namespace x))))
(defn- sym-set? [x] (and (set? x) (every? unqualified-symbol? x)))

(def scalar? (some-fn number? true? false? string? keyword? nil?))

;; Maybe this will only be useful when optimizing
(defn- form-deps
  "Returns a set of locals (symbols) on which the form depends."
  [form locals]
  {:pre [(sym-set? locals)]
   :post [(sym-set? %)]}
  (cond
   
   (scalar? form)
   #{}
   
   (symbol? form)
   (if (locals form) #{form} #{})

   (= 'if (first form))
   (apply sets/union (map form-deps (rest form)))))

(defn- satisfied?
  [bindings sym-set]
  (sets/subset? sym-set (set (keys bindings))))

(defn- quote'
  [form]
  (list 'quote form))

(defn if-computer-helper
  [bindings if-name cond-name t-name f-name condition t f]
  (cond

   ;; We've already computed this
   (contains? bindings if-name)
   bindings

   ;; We've already computed the condition at least
   (contains? bindings cond-name)
   (if (bindings cond-name)
     (let [bindings (t bindings)]
       (if (contains? bindings t-name)
         (assoc bindings if-name (bindings t-name))
         bindings))
     (let [bindings (f bindings)]
       (if (contains? bindings f-name)
         (assoc bindings if-name (bindings f-name))
         bindings)))

   :else
   (let [bindings (condition bindings)]
     (if (contains? bindings cond-name)
       (if-computer-helper bindings if-name cond-name t-name f-name condition t f)
       bindings))))

(defn function-call-computer-helper
  [bindings call-name sub-computers names]
  (cond

   (contains? bindings call-name)
   bindings

   :else
   (let [bindings ((apply comp sub-computers) bindings)]
     (if (every? #(contains? bindings %) names)
       (assoc bindings
         call-name
         (apply (bindings (first names))
                (map bindings (rest names))))
       bindings))))


;; surely this must be some sort of monad.
(defn- computer
  "Returns a form for (fn [bindings]) => bindings"
  [args form form-name]
  (cond

   (symbol? form)
   (if (some #{form} args)
     `(fn [bindings#]
        (if (bindings# '~form)
          (assoc bindings# '~form-name (bindings# '~form))
          bindings#))
     `(fn [bindings#]
        (assoc bindings# '~form-name ~form)))

   (scalar? form)
   `(fn [bindings#]
      (assoc bindings# '~form-name ~form))

   (= 'if (first form))
   (let [[_ cond t f] form
         cond-name (gensym "condition_")
         t-name (gensym "t_")
         f-name (gensym "f_")
         [cond-computer t-computer f-computer] (for [[form' name] [[cond cond-name]
                                                                   [t t-name]
                                                                   [f f-name]]]
                                                 (computer args form' name))]
     `(fn [bindings#]
       (if-computer-helper
        bindings#
        '~form-name
        '~cond-name
        '~t-name
        '~f-name
        ~cond-computer
        ~t-computer
        ~f-computer)))

   ;; At this point we assume it is a regular function call and not
   ;; a special form
   (seq? form)
   (let [names (for [x form] (gensym "funcall_parts_"))]
     `(fn [bindings#]
        (function-call-computer-helper
         bindings#
         '~form-name
         ~(vec (for [[x name] (map vector form names)]
                 (computer args x name)))
         '~names)))))

(defn arg-eater
  [bindings remaining-args form-name computer]
  (fn [& args]
    (loop [bindings bindings, remaining-args remaining-args, args args]
      (if (empty? remaining-args)
        (let [bindings (computer bindings)]
          (if (contains? bindings form-name)
            (bindings form-name)
            (throw (Exception. (str "CRAP: " form-name)))))
        (if (empty? args)
          (arg-eater bindings remaining-args form-name computer)
          (recur
           (computer (assoc bindings (first remaining-args) (first args)))
           (rest remaining-args)
           (rest args)))))))

(defmacro curried-fn
  [arg-list body]
  (let [body (wk/macroexpand-all body)
        form-name (gensym "outer_form_")]
    `(arg-eater
      {}
      '~arg-list
      '~form-name
      ~(computer arg-list body form-name))))
