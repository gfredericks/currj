(ns currj.test.core
  (:use [currj.core])
  (:use [clojure.test]))

(defn- count-calls
  [func]
  (let [a (atom 0)]
    (with-meta
      (fn [& args] (swap! a inc) (apply func args))
      {::count-check (partial deref a)})))

(defn- called
  [func]
  ((-> func meta ::count-check)))

(deftest minimal-example-test
  (let [pos? (count-calls pos?)
        f (curried-fn [x y] (if (pos? x) (inc y) (dec y))),
        g (f -12)]
    (is (= 13 (g 14)))
    (is (= 1 (called pos?)))
    (is (= 813 (g 814)))
    (is (= 1 (called pos?)))))

(deftest function-arg-test
  (let [+ (count-calls +)
        f (curried-fn [x y] (* y (+ 10 x)))
        g (f 10)]
    (is (= 100 (g 5)))
    (is (= 1 (called +)))
    (is (= 120 (g 6)))
    (is (= 1 (called +)))))

#_(deftest let-test
  (let [pos? (count-calls pos?)
        + (count-calls +)
        f (curried-fn [x y]
                      (let [z (+ x 12)]
                        (* z y)))]))