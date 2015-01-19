;;; Excercise 1.3
;;; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square n) (* n n))
(define (sum-of-squares a b) (+ (square a) (square b)))
(define (f a b c)
  (cond 
    ((and (> a c) (> b c)) (sum-of-squares a b))
    ((and (> a b) (> c b)) (sum-of-squares a c))
    (else (sum-of-squares b c))))

(if (= 25 (f 1 3 4)) "Test Passed" "Test failed")
(if (= 25 (f 3 1 4)) "Test Passed" "Test failed")
(if (= 25 (f 4 3 1)) "Test Passed" "Test failed")

;;; Excercise 1.7
;;; Design a square root procedure that stops when the percentage

(define (div a b) (/ a b))
(define (good-enough? number guess) (> (* number .001) (abs (- number (square guess)))))
(define (avg x y) (div (+ x y) 2)) 
(define (improve number guess) (avg guess (div number guess)))
(define (sqrt number guess)
  (if (good-enough? number guess)
    (exact->inexact guess)
    (sqrt number (improve number guess))))

(sqrt 9 1)

;;; Excercise 1.8
;;; Design a similar program for cube roots: given formula (number / guess^2 + 2guess) / 3
(define (cube n) (* n (square n)))
(define (good-enough-cb? number guess) (> (* number .001) (abs (- number (cube guess)))))
(define (improve-cb number guess)
  (/ (+ (/ number (square guess)) (* 2 guess)) 3))

(define (cbrt-abs number guess)
  (if (good-enough-cb? number guess)
    (exact->inexact guess)
    (cbrt-abs number (improve-cb number guess))))

(define (cbrt number guess)
  (define result-abs (cbrt-abs (abs number) (abs guess)))
  (if (> 0 number) (* -1 result-abs) result-abs))

(cbrt 8 1)
(cbrt 16 1)
(cbrt -16 1)