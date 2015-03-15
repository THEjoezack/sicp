(define (assert a b) (if (= a b) "Test Passed" "Test Failed") ) ;;; Helpful util method :)



;;; Excercise 1.3
;;; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square n) (* n n))
(define (sum-of-squares a b) (+ (square a) (square b)))
(define (f a b c)
  (cond 
    ((and (> a c) (> b c)) (sum-of-squares a b))
    ((and (> a b) (> c b)) (sum-of-squares a c))
    (else (sum-of-squares b c))))

(assert 25 (f 1 3 4))
(assert 25 (f 3 1 4))
(assert 25 (f 4 3 1))



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

(assert 3 (round (sqrt 9 1)))
(assert 4 (round (sqrt 16 1)))



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

(assert 2 (round (cbrt 8 1)))
(assert 3 (round (cbrt 16 1)))
(assert -3 (round (cbrt -16 1)))



;;; Excercise 1.11
;;; Program both a recursive and iterative solution to the following equation
;;; If n < 3 ? n : f(n - 1) + 2*f(n - 2) + 3*f(n - 3)

(define (recursive n)
  (if (< n 3)
    n
    (+
      (recursive (- n 1))
      (* 2 (recursive (- n 2)))
      (* 3 (recursive (- n 3))))))

(assert -31 (recursive -31))
(assert 2 (recursive 2))
(assert 4 (recursive 3))
(assert 11 (recursive 4))
(assert 338870 (recursive 16))

(define (iterative count)
  (define (iter prev prev-1 prev-2 current)
    (define (get-sum) (+ prev (* 2 prev-1) (* 3 prev-2)))
    (cond
      ((< count 3) count)
      ((= current count) prev)
      (else (iter (get-sum) prev prev-1 (+ current 1)))))
  (iter 2 1 0 2))

(define (rec-vs-iter n) (assert (recursive n) (iterative n)))
(rec-vs-iter -31)
(rec-vs-iter 2)
(rec-vs-iter 3)
(rec-vs-iter 4)
(rec-vs-iter 16)

;;; Excercise 1.12
;;; Pascals triangle, recursive

(define (pascal row column)
  (if (or (= column 0) (= column row))
    1
    (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column))))

(assert 1 (pascal 0 0))
(assert 1 (pascal 1 1))
(assert 2 (pascal 2 1))
(assert 6 (pascal 4 2))

;;; Excercise 1.16
;;; Iterative exponentiation using successive squaring

(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (define (expt-iter b n a)
    (cond
      ((= n 0) a)
      ((even? n) (* b (expt-iter b (/ n 2) a)))
      (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

(assert 8 (expt 2 3))
(assert 100 (expt 10 2))
(assert 1 (expt 2 0))
