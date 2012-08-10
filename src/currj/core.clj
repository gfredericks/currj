(ns currj.core
  (:refer-clojure :exclude [fn])
  (:require [clojure.tools.macro :as macro]
            [currj.independence :as ci]))

(defmacro fn
  [arg-list body]
  (let [[lettings form] (ci/refactor (macro/mexpand-all body) arg-list)
        fn-form
        `(clojure.core/fn
          ~@(for [n (range 1 (count arg-list))
                  :let [[given not-given] (split-at n arg-list)]]
              `([~@given]
                  (fn [~@not-given] ~form)))
          ([~@arg-list] ~form))]
    (if (empty? lettings)
      fn-form
      `(let [~@(apply concat lettings)]
         ~fn-form))))
