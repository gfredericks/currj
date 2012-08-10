# currj

currj is an experiment in currying and Clojure. It currently attempts
to support function calls, if, and let. It's not very well tested, so
if you want to use it for something serious you should probably check
the output manually.

## Goals

1. Support for all pure functions.
2. Emit code that is no less efficient than a hand-curried function.

## Usage

``` clojure
(require '[currj.core :as currj])

(def f (currj/fn [x y] (+ y (* 2 x))))
(def g (f 5))

(g 3) ;; => 13
(g -8) ;; => 2
;; the * function was only called once
```

## TODO

* Implement `fn*`

## License

Copyright (C) 2012 Gary Fredericks

Distributed under the Eclipse Public License, the same as Clojure.
