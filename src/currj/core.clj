(ns currj.core
  (:refer-clojure :exclude [fn])
  (:require [clojure.tools.macro :as macro]
            [currj.independence :as ci]))

(defmacro fn
  [arg-list body]
  (let [[lettings form] (ci/refactor (macro/mexpand-all body) arg-list)
        partial-bodies (for [n (range 1 (count arg-list))
                             :let [[given not-given] (split-at n arg-list)]]
                         `([~@given]
                             (fn [~@not-given] ~form)))
        fn-form
        (if (empty? partial-bodies) ;; for more readable output
          `(clojure.core/fn [~@arg-list] ~form)
          `(clojure.core/fn
            ~@partial-bodies
            ([~@arg-list] ~form)))]
    (if (empty? lettings)
      fn-form
      `(let [~@(apply concat lettings)]
         ~fn-form))))
