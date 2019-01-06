# Power Assertion macro for Clojure

I love the [Power Assertions](http://groovy-lang.org/testing.html#_power_assertions) feature in the Groovy language. It makes writing tests and finding bugs easier. This is an implementation for the Clojure programming language with support for macros and lazy sequences.

It prints subexpressions on assert failure.

<img src="docs/animation-1.gif"/>

And each evaluations of subexpressions are printed.

<img src="docs/animation-3.gif"/>

[![Clojars Project](https://img.shields.io/clojars/v/io.github.erdos/erdos.assert.svg)](https://clojars.org/io.github.erdos/erdos.assert)
[![EPL 1.0](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://www.eclipse.org/legal/epl-1.0/)

## Usage

This library provides two small macros for easier debugging.
 - The `examine` macro can be used to trace parts of an evaluated expression. Debug information is printed to the standard output and the value of the expression is returned.
 - The `assert` macro is similar to `clojure.core/assert` but it also wraps the examined information in the thrown `AssertionError` instance.


**First**, add the dependency to your `project.clj`.

``` clojure
[erdos.assert "0.1.0"]

```


**Second**, require the namespace:

``` clojure
(require '[erdos.assert :as ea])
```


In the REPL, **examining** simple expressions will print to `*out*`.

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

Only the already realized part is printed for lazy sequences.

```clojure
erdos.assert=> (examine (reduce + (map * (range) (range 0 10 2))))

(reduce + (map * (range) (range 0 10 2)))
¦         ¦      ¦       ¦
60        (…)    (0 …)   (0 2 4 6 8) 
```

In such cases you might want to run [doall](https://clojuredocs.org/clojure.core/doall) on intermediate lazy sequences.

```clojure
erdos.assert=> (examine (reduce + (doall (map * (range) (range 0 10 2)))))

(reduce + (doall (map * (range) (range 0 10 2))))
¦         ¦      ¦      ¦       ¦
60        ¦      (…)    (0 …)   (0 2 4 6 8) 
          (0 2 8 18 32) 
```

## License

Copyright © 2019 Janos Erdos

Distributed under the [Eclipse Public License version 1.0](https://www.eclipse.org/legal/epl-1.0/).
