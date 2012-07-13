# currj

currj is an experiment in currying and Clojure. It currently supports function
calls and if.

## Goals

1. Support for all pure functions
2. Emit code that is no less efficient than a hand-curried function.

## Usage

``` clojure
(use '[currj.core :only [curried-fn]])

(def f (curried-fn [x y] (+ y (* 2 x))))
(def g (f 5))

(g 3) ;; => 13
(g -8) ;; => 2
;; the * function was only called once
```

## TODO

* Implement `let*`
* Implement `fn*`

## License

Copyright (C) 2012 Gary Fredericks

Distributed under the Eclipse Public License, the same as Clojure.
