#lang racket
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x) (if (< x 0) (- x) x))
(define (average x y) (/ (+ x y) 2))
(define (average3 x y z) (/ (+ x y z) 3))

(define (sqrt x)
  (define (good-enough guess) 
    (< (abs (- (square guess) x)) 0.1))
  
  (define (good-enough2 old guess) 
    (< (abs (- old guess)) 0.01))
  
  (define (improve-guess guess) 
    (average guess (/ x guess)))
  
  (define (sqrt-iter2 old guess) 
    (if (good-enough2 old guess)
     guess
     (sqrt-iter2 guess (improve-guess guess))))
  
  (define (sqrt-iter guess)
    (if (good-enough guess x) 
     guess
     (sqrt-iter (improve-guess guess))))
  
  (sqrt-iter2 0 1.0))


(define (cube-root x)
  (define (good-enough-cube guess) (< (abs (- (cube guess) x)) 0.1))
  
  (define (cube-impr guess) (/ (+ (* 2 guess) (/ x (square guess))) 3))
  
  (define (cube-root-iter guess) (if (good-enough-cube guess)
                                  guess
                                  (cube-root-iter (cube-impr guess))))
  (cube-root-iter 1.0))

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
(define (fib-iter n)
  (define (helper n2 n1 counter)
    (cond ((= counter 0) n1)
          (else (helper n1 (+ n1 n2) (- counter 1)))))
  (helper 0 1 n))

;Counting Coins Problem: Given 1,5,10,25,50 cent denominations, find number of permutations of a coin make up an amount
;Added restriction of only unique coin permutations.
(define (count-coin dollars)
  (define (helper denom cents ways) 
    (cond ((= cents 0) ways)
          ((< cents 0) 0)
          ((= denom 50) (+ (helper denom (- cents denom) (+ ways 1)) 
                           (helper 25 (- cents 25) (+ 1 ways)) 
                           (helper 10 (- cents 10) (+ 1 ways)) 
                           (helper 5 (- cents 5) (+ 1 ways)) 
                           (helper 1 (- cents 1) (+ 1 ways))))
          ((= denom 25) (+ (helper denom (- cents denom) (+ ways 1)) 
                           (helper 10 (- cents 10) (+ 1 ways)) 
                           (helper 5 (- cents 5) (+ 1 ways)) 
                           (helper 1 (- cents 1) (+ 1 ways))))
          ((= denom 10) (+ (helper denom (- cents denom) (+ ways 1)) 
                           (helper 5 (- cents 5) (+ 1 ways)) 
                           (helper 1 (- cents 1) (+ 1 ways))))
          ((= denom 5) (+ (helper denom (- cents denom) (+ ways 1)) 
                           (helper 1 (- cents 1) (+ 1 ways))))
          (else (+ (helper denom (- cents denom) (+ ways 1))))))
  (helper 50 (* 100 dollars) 0))

(define (count-coin-iter dollars)
  (define (helper fifty quarter dime nickel penny denom ways)
    (cond ((and (= denom 1) (>= penny 5)) (helper fifty quarter dime (+ 1 nickel) (- penny 5) denom (+ ways 1)))
          ((and (= denom 1) (< penny 5)) (helper fifty quarter dime nickel penny 5 ways))
          ((and (= denom 5) (>= nickel 2)) (helper fifty quarter (+ 1 dime) (- nickel 2) penny denom (+ ways 1)))
          ((and (= denom 5) (< nickel 2)) (helper fifty quarter dime nickel penny 10 ways))
          ((and (= denom 10) (>= dime 3)) (helper fifty (+ 1 quarter) (- dime 3) (+ nickel 1) penny denom (+ ways 1)))
          ((and (= denom 10) (= dime 2) (>= nickel 1)) (helper fifty (+ 1 quarter) (- dime 2) (- nickel 1) penny denom (+ ways 1)))
          ((and (= denom 10) (< dime 2)) (helper fifty quarter dime nickel penny 25 ways))
          ((and (= denom 25) (>= quarter 2)) (helper (+ 1 fifty) (- quarter 2) dime nickel penny 25 (+ 1 ways)))
          (else ways)))
  (helper 0 0 0 0 (* 100 dollars) 1 1))

;How many combinations of (all denominations except 1) exist
(define (count-coin-iter2 dollars)
  (define (is-valid fifty quarter dime nickel)
    (<= (+ (* 50 fifty) (* 25 quarter) (* 10 dime) (* 5 nickel)) (* 100 dollars)))
  (define (helper fifty quarter dime nickel ways denom)
    (cond ((= 5 denom) 
           (if (is-valid fifty quarter dime (+ 1 nickel))
               (helper fifty quarter dime (+ 1 nickel) (+ 1 ways) denom)
               (helper fifty quarter (+ 1 dime) 0 ways 10)))
          ((= 10 denom) 
           (if (is-valid fifty quarter dime nickel)
               (helper fifty quarter dime 0 (+ 1 ways) 5)
               (helper fifty (+ 1 quarter) 0 0 ways 25)))
          ((= 25 denom) 
           (if (is-valid fifty quarter dime nickel)
               (helper fifty quarter 0 0 (+ 1 ways) 5)
               (helper (+ 1 fifty) quarter 0 0 ways 50)))
          ((= 50 denom) 
           (if (is-valid fifty quarter dime nickel)
               (helper fifty 0 0 0 (+ 1 ways) 5)
               ways))))
  (helper 0 0 0 0 1 5))

(define (Ex1.11 n)
  (cond ((< n 3) n)
        (else (+ (Ex1.11 (- n 1)) (* 2 (Ex1.11 (- n 2))) (* 3 (Ex1.11 (- n 3)))))))

(define (Ex1.11_iter n)
  (define (helper c b a count)
    (if (= count 0)
        (+ c (* 2 b) (* 3 a))
        (helper (+ c (* 2 b) (* 3 a)) c b (- count 1))))
  (if (< n 3)
      n
      (helper 2 1 0 (- n 3))))

(define (pascal x)
  (define (coord-to-int row pos)
    (define (row-to-int row acc)
      (if (= row 0)
          acc
          (row-to-int (- row 1) (+ acc row))))
    (+ pos (- (row-to-int row 0) row)))
  (define (row x)
    (define (row-helper level x)
      (if (<= x level) 
          level
          (row-helper (+ 1 level) (- x level))))
    (if (= 1 x) 
        1
        (row-helper 1 x)))
  (define (pos x)
    (define (pos-helper level x)
      (if (<= x level) 
          x
          (pos-helper (+ 1 level) (- x level))))
    (if (= 1 x) 
        1
        (pos-helper 1 x)))
  (define (edge x)
    (define (left-edge level x)
      (cond ((< x 1) 1)
            ((= x 1) 0)
            (else (left-edge (+ 1 level) (- x level)))))
    (define (right-edge level x)
      (cond ((< x 1) 1)
            ((= x 1) 0)
            (else (right-edge (+ 1 level) (- x (+ 1 level))))))
    (if (< x 4)
        0
        (* (left-edge 1 x) (right-edge 1 x))))
  
  (cond ((<= x 0) 0)
         ((= 0 (edge x)) 1)
        (else (+ (pascal (coord-to-int (- (row x) 1) (- (pos x) 1))) (pascal (coord-to-int (- (row x) 1) (pos x)))))))
;        (+ (pascal (coord-to-int (- (row x) 1) (- (pos x) 1))) (pascal (coord-to-int (- (row x) 1) (pos x))))))