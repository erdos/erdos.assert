# erdos.assert

Power assert macro for Clojure.

## Usage

This library provides two small macros for easier debugging.
 - The `examine` macro can be used to trace parts of an evaluated expression. Debug information is printed to the standard output and the value of the expression is returned.
 - The `assert` macro is similar to `clojure.core/assert` but it also wraps the examined information in the thrown `AssertionError` instance.


**First**, add the dependency to your `project.clj`.

``` clojure
[erdos.assert "0.1.0-SNAPSHOT"]

```

**Second**, require the namespace:

``` clojure
(require '[erdos.assert :as ea])`
```

**Examining** simple expressions will print to `*out*`.

``` clojure
$ (ea/examine (* (+ 19 17) (- 19 17)))
; (* (+ 19 17) (- 19 17))
; ¦  ¦         ¦
; 72 36        2

```

You can also write **assertions** that will wrap examined data as a string in the `AssertionError` instance.


``` clojure
$ (ea/assert (= 1 (* 3 (/ 1 3)) "") ; does not print anything

$ (ea/assert (= (* 1.0 1) (* 3 (/ 1 3))) "")
; AssertionError
; (= (* 1.0 1) (* 3 (/ 1 3)))
; ¦  ¦         ¦    ¦
; ¦  1.0       1N   1/3
; false
```


Shown output is **arranged** to make place for more complex output.

``` clojure
$ (ea/examine (+ (* 12 (- 32 12) 12.2) (- 3 (/ 1 2 3 4) 2.2) (* (- 1 2) 3)))
; (+ (* 12 (- 32 12) 12.2) (- 3 (/ 1 2 3 4) 2.2) (* (- 1 2) 3))
; ¦  ¦     ¦               ¦    ¦                ¦  ¦
; ¦  ¦     20              ¦    1/24             -3 -1
; ¦  2928.0                0.7583333333333329
; 2925.758333333333
```


Some expressions are evaluated **multiple times**, hence all values are shown.

``` clojure
$ (ea/examine (dotimes [i 5] (print (* i i))))
; (dotimes [i 5] (print (* i i)))
;                ¦      ¦
;                ¦      16, 9, 4, 1, 0
;                nil, nil, nil, nil, nil
```


## License

Copyright © 2018 Janos Erdos

Distributed under the Eclipse Public License either version 1.0.
