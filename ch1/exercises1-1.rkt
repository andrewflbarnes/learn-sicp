#lang racket

;; From: https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html

;; Useful functions

(define (sq x)
  (* x x))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average x y)
  (/ (+ x y) 2))

;; Exercises

"========================================================="
"Exercise 1.1: N/A"

"========================================================="
"Exercise 1.2"
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))

"========================================================="
"Exercise 1.3"
(define (ex13 x y z)
  (define (result x y)
    (+ (sq x) (sq y)))
  (cond ((and (< x y) (< x z)) (result y z))
        ((< y z) (result x z))
        (else (result x y))))
"Input: Variation of 2 3 4"
ex13
(ex13 2 3 4)
(ex13 2 4 3)
(ex13 3 2 4)
(ex13 3 4 2)
(ex13 4 2 3)
(ex13 4 3 2)

"========================================================="
"Exercise 1.4: N/A"

"========================================================="
"Exercise 1.5"
"Applicative Order: Program will hang as it recursively resolves (p)"
"Normal Order: Program will emit 0 as (p) is never evaluated"

"========================================================="
"Exercise 1.6"
"Applicative order evaluation means to evaluate new-if we must evaluate a call to sqrt-iter"
"which has another call to new-if resulting in infinite recursion."

"========================================================="
"Exercise 1.7"
(define (sqrt x)
  (define (good-enough? guess lastguess)
    (< (abs (- guess lastguess)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess lastguess x)
    (if (good-enough? guess lastguess)
        guess
        (sqrt-iter (improve guess x)
                   guess
                   x)))
  (sqrt-iter 1.0 0.0 x))
"sqrt 2"
(sqrt 2)
"sqrt 10000000000000000"
(sqrt 10000000000000000)
"sqrt 0.000001"
(sqrt 0.000001)

"========================================================="
"Exercise 1.8"
(define (cbrt x)
  (define (good-enough? guess lastguess)
    (< (abs (- guess lastguess)) 0.0001))
  (define (improve guess x)
    (/ (+ (/ x (sq guess)) (* 2 guess)) 3))
  (define (cbrt-iter guess lastguess x)
    (if (good-enough? guess lastguess)
        guess
        (cbrt-iter (improve guess x)
                   guess
                   x)))
  (cbrt-iter 1.0 0.0 x))
"cbrt 2"
(cbrt 2)
"cbrt 1000000000000000"
(cbrt 1000000000000000)
"cbrt 0.000000001"
(cbrt 0.000000001)


"========================================================="
"Supplemental -> see code"
;; abstract root defines
(define (rt improve)
  (define (good-enough? guess lastguess)
    (< (abs (- guess lastguess)) 0.0001))
  (define (rt-iter guess lastguess)
    (if (good-enough? guess lastguess)
        guess
        (rt-iter (improve guess)
                 guess)))
  (rt-iter 1.0 0.0))

(define (sqrt2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (rt improve))

(define (cbrt2 x)
  (define (improve guess)
    (/ (+ (/ x (sq guess)) (* 2 guess)) 3))
  (rt improve))