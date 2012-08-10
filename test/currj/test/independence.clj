(ns currj.test.independence
  (:refer-clojure :exclude [fn])
  (:use currj.independence
        clojure.test))

(defmacro fn
  [args body]
  (let [[lettings body] (refactor body args)]
    `(let [~@(apply concat lettings)]
       (clojure.core/fn ~args ~body))))

(deftest trivials
  (are [foo] (= [[] foo] (refactor foo '[snow bob]))
       5
       :hehe
       "yep"
       [:this :evals :to "itself"]
       true
       false
       nil
       'snow
       'bob))

(deftest if-test
  (let [[lettings form] (refactor '(if (+ a b 17) snow bob) '[snow bob])]
    ;; should look like [[[g_123 (+ a b 17)]] (if g_123 snow bob)].
    ;; I need a better way to express that.

    (is (= 1 (count lettings)))
    (is (= '(+ a b 17) (-> lettings first second)))
    (let [[_if cond truethy falsy] form]
      (is (= 'if _if))
      (is (symbol? cond))
      (is (= 'snow truethy))
      (is (= 'bob falsy)))))

(deftest fn-call-test
  (is (= 2 ((fn [a b] (+ a (- 7 3) b)) -1 -1)))
  (let [[lettings form] (refactor '(+ a (- 7 3) b) '[a b])]
    (is (not (empty? lettings)))
    (is (-> lettings first second (= '(- 7 3))))
    (is (every? symbol? form))))