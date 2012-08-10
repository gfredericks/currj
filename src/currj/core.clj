(ns currj.core
  (:refer-clojure :exclude [fn])
  (:require [clojure.walk :as wk]
            [currj.independence :as ci]))

(defmacro fn
  [arg-list body]
  (let [[lettings form] (ci/refactor (wk/macroexpand-all body) arg-list)]
    `(let [~@(apply concat lettings)]
       (clojure.core/fn
        ~@(for [n (range 1 (count arg-list))
                :let [[given not-given] (split-at n arg-list)]]
            `([~@given]
                (fn [~@not-given] ~form)))
        ([~@arg-list] ~form)))))
