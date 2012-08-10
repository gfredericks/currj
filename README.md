# currj

currj is an experiment in currying and Clojure. It currently attempts
to support function calls, if, and let. It's not very well tested, so
if you want to use it for something serious you should probably check
the output manually.

## Goals

1. Support for all pure functions.
2. Emit code that is no less efficient than a hand-curried function.
3. Support ClojureScript

## Usage

``` clojure
(require '[currj.core :as currj])

(def f (currj/fn [x y] (+ y (* 2 x))))
(def g (f 5))

(g 3) ;; => 13
(g -8) ;; => 2
;; the * function was only called once
```

## Example Expansion

### Small Example

``` clojure
;; original

(currj/fn [a b]
  (+ b (* (dec a)
          (+ 88 42))))

;; simplified (readable) expansion

(let
 [G__2256 (+ 88 42)]
 (fn
  ([a] (let [G__2257 (* (dec a) G__2256)]
         (fn [b] (+ b G__2257))))
  ([a b] (+ b (* (dec a) G__2256)))))

;; actual expansion

(let*
 [G__2256 (+ 88 42)]
 (fn*
  ([a] (let* [G__2257 (* (dec a) G__2256)] (fn* ([b] (+ b G__2257)))))
  ([a b] (+ b (* (dec a) G__2256)))))
```

### Bigger Example

``` clojure
;; original

(currj/fn
 [a b c]
 (let [d (+ a b)
       my-constant (* 2 Math/PI)]
   (if (pos? a)
     (- c (* d d))
     (let [e (inc a)] (/ e c)))))

;; simplified (more readable) expansion

(let
 [my-constant (* 2 Math/PI)]
 (fn
  ([a]
   (let
    [if-condition2492 (pos? a)
     e (if-not if-condition2492
         (inc a)
         nil)]
    (fn
     ([b]
      (let
       [d (+ a b)
        if-condition2494 if-condition2492
        G__2495 (if if-condition2494 (* d d))]
       (fn ([c] (if if-condition2494
                  (- c G__2495)
                  (/ e c))))))
     ([b c]
      (let
       [d (+ a b)]
       (if if-condition2492
         (- c (* d d))
         (/ e c)))))))
  ([a b]
   (let
    [d (+ a b)
     if-condition2497 (pos? a)
     G__2498 (if if-condition2497 (* d d))
     e (if-not if-condition2497
         (inc a)
         nil)]
    (fn ([c] (if if-condition2497
               (- c G__2498)
               (/ e c))))))
  ([a b c]
   (let
    [d (+ a b)]
    (if (pos? a)
      (- c (* d d))
      (let [e (inc a)]
        (/ e c)))))))

;; actual expansion

(let*
 [my-constant (* 2 Math/PI)]
 (fn*
  ([a]
   (let*
    [if-condition2492
     (pos? a)
     e
     (if (clojure.core/not if-condition2492) (inc a) nil)]
    (fn*
     ([b]
      (let*
       [d
        (+ a b)
        if-condition2494
        if-condition2492
        G__2495
        (if if-condition2494 (* d d))]
       (fn* ([c] (if if-condition2494 (- c G__2495) (/ e c))))))
     ([b c]
      (let*
       [d (+ a b)]
       (if if-condition2492 (- c (* d d)) (/ e c)))))))
  ([a b]
   (let*
    [d
     (+ a b)
     if-condition2497
     (pos? a)
     G__2498
     (if if-condition2497 (* d d))
     e
     (if (clojure.core/not if-condition2497) (inc a) nil)]
    (fn* ([c] (if if-condition2497 (- c G__2498) (/ e c))))))
  ([a b c]
   (let*
    [d (+ a b)]
    (if (pos? a) (- c (* d d)) (let* [e (inc a)] (/ e c)))))))
```

## TODO

* Implement `fn*`

## License

Copyright (C) 2012 Gary Fredericks

Distributed under the Eclipse Public License, the same as Clojure.
