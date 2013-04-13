#lang racket
(define size 2)
(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (sum-max-squares x y z)
  (cond ((>= x y)
         (if (> z y) (sum-of-square x z) (sum-of-square x y)))
        ((>= y z)
         (if (> z x) (sum-of-square z y) (sum-of-square y x)))
        (else
         (if (> x y) (sum-of-square z x) (sum-of-square z y)))))

(define (average x y) (/ (+ x y) 2))
(define (better-guess guess x) (average guess (/ x guess)))
(define (abs x) (if (>= x 0) x (* -1 x)))
(define (square-diff guess x) (abs (- (square guess) x)) )
(define (good-enough guess x) (< (square-diff guess x) 0.00001))
(define (sqrt-iter guess x) 
  (if (good-enough guess x) 
      guess
      (sqrt-iter (better-guess guess x) x)))
     