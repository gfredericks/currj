(ns currj.test.core
  (:require [currj.core :as currj])
  (:use [clojure.test]))

(defn- count-calls
  [func]
  (let [a (atom 0)]
    (with-meta
      (fn [& args] (swap! a inc) (apply func args))
      {::counter a})))

(defn- reset-counts!
  [& funcs]
  (doseq [func funcs]
    (-> func meta ::counter (reset! 0))))

(defn- called
  [func]
  (-> func meta ::counter deref))

(deftest works-like-normal-test
  (let [f (currj/fn [a b c] (- (b c) a))]
    (is (= (f 7 inc 9) 3))))

(deftest no-nullary-version-test
  (is (thrown? clojure.lang.ArityException ((currj/fn [x y] (+ x y))))))

(deftest minimal-example-test
  (let [pos? (count-calls pos?)
        f (currj/fn [x y] (if (pos? x) (inc y) (dec y))),
        g (f -12)]
    (is (= 13 (g 14)))
    (is (= 1 (called pos?)))
    (is (= 813 (g 814)))
    (is (= 1 (called pos?)))))

(deftest function-arg-test
  (let [+ (count-calls +)
        f (currj/fn [x y] (* y (+ 10 x)))
        g (f 10)]
    (is (= 100 (g 5)))
    (is (= 1 (called +)))
    (is (= 120 (g 6)))
    (is (= 1 (called +)))))

(deftest gradual-test
  (let [malled #(map called %&)
        dinc (count-calls (comp inc inc))
        [+ - * / inc dec < +'] (map count-calls [+ - * / inc dec < +])
        one? (partial = 1)
        f (currj/fn [w x y z]
                    (if (< w (dec 8))
                      (+ w z (+' 3 4))
                      (* z (- (/ w y) (dinc 18) (inc x)))))]
    (is (one? (called dec)))
    (is (every? zero? (malled + - * / inc < dinc +')))
    (let [g (f 17)]
      (is (every? one? (malled < dinc dec)))
      (is (every? zero? (malled + - * / inc +')))
      (let [h (g 6)]
        (is (every? one? (malled < dinc inc dec)))
        (is (every? zero? (malled + - * / +')))
        (let [i (h 12)]
          (is (every? one? (malled < dinc inc / - dec)))
          (is (every? zero? (malled + * +')))
          (is (= -3991/12 (i 13)))
          (is (every? one? (malled < dinc inc / - * dec)))
          (is (every? zero? (malled + +'))))))
    (reset-counts! dinc + - * / inc dec < +')
    (let [g (f 4)]
      (is (every? one? (malled < +')))
      (is (every? zero? (malled dinc + - * / inc dec)))
      (is (= 19 (g 1 2 8)))
      (is (every? one? (malled < +' +)))
      (is (every? zero? (malled dinc - * / inc dec))))))

(deftest let-test
  (let [pos? (count-calls pos?)
        + (count-calls +)
        f (currj/fn [x y]
                    (let [z (+ x 12)]
                      (* z y)))
        g (f 19)]
    (is (= 31 (g 1)))
    (is (= 1 (called +)))
    (is (= 62 (g 2)))
    (is (= 1 (called +)))))

(deftest quote-test
  (let [f (currj/fn [a b] (list* a b '(1 2 3 4)))]
    (is (= ((f 8) 9) '(8 9 1 2 3 4)))))

(deftest shadowing-quote-test
  (let [f (currj/fn [a b] (let [a (* 2 a)] (list* a b '(a b c))))]
    (is (= ((f 8) 9) '(16 9 a b c)))))