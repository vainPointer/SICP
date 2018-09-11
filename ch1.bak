#lang sicp

; 1.1
(define square (lambda (x) (* x x)))
(define abs (lambda (x)
              (cond ((> x 0) x)
                    ((= x 0) 0)
                    ((< x 0) (- x)))))
(define big-two (lambda (a b c)
                  (if (< a b)
                      (if (< a c) (+ b c) (+ a b))
                      (if (< b c) (+ a c) (+ a b)))))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; 测试是否为正则序
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

; 牛顿法求平方根
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
  
(define (sqrt2 x)
  (define (good-enough? guess pre-guess)
    (< (abs (- guess pre-guess)) 0.001))
  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess pre-guess x)
    (if (good-enough? guess pre-guess)
        guess
        (sqrt-iter (improve guess x) guess x)))  
  (sqrt-iter 1.0 2.0 x))

; 1.2
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))      ; 递归
(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))                          ; 尾递归

; Ex 1.11
(define (fib123 n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* b 2) (* c 3)) a b (- count 1))))
    (iter 2 1 0 n))

; 迭代版换零钱
(define (count-change-iterative amount)
  ;; penny is not in the signiture, bacause it equals (- amount else)
  (define (count-iter sum half-dollar quarter dime nickeli)
    (cond ((> (* half-dollar 50) amount) sum)
          ((> (+ (* half-dollar 50)
                 (* quarter 25)) amount)
           (count-iter sum (+ half-dollar 1) 0 0 0))
          ((> (+ (* half-dollar 50)
                 (* quarter 25)
                 (* dime 10)) amount)
           (count-iter sum half-dollar (+ quarter 1) 0 0))
          ((> (+ (* half-dollar 50)
                 (* quarter 25)
                 (* dime 10)
                 (* nickeli 5)) amount)
           (count-iter sum half-dollar quarter (+ dime 1) 0))
          (else (count-iter (+ 1 sum) half-dollar quarter dime (+ nickeli 1)))))
  (count-iter 0 0 0 0 0))               ; 这个代码太酷了，学习了
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Ex 1.12
(define (pascal x y)
  (cond ((= y 1) 1)
        ((= y 2) 1)
        ((= x 1) 1)
        ((= x y) 1)
        (else (+ (pascal (- x 1) (- y 1))
                 (pascal x (- y 1))))))

; Ex 1.16
(define (fast-expt1 b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter
		    a
		    (* b b)
		    (/ n 2)))
	(else (fast-expt-iter
	       (* a b)
	       b
	       (- n 1)))))

; Ex 1.23
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

; Ex 1.28
(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (square-check (expmod2 base (/ exp 2) m) m))
	(else (remainder (* base (expmod2 base (- exp 1) m)) m))))

(define (square-check x m)  
  (if (and (not (or (= x 1) (= x (- m 1))))
	   (= (remainder (square x) m) 1))
      0                     ; x为模m的1的非平凡平方根，则m不是素数
      (remainder (square x) m)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime2? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime2? n (- times 1)))
        (else false)))

; Ex 1.32 accumulate过程
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

; Ex 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (define (next-filtered x)
    (define n (next x))
    (if (> n b)
	n
	(if (filter n)
	    n
	    (next-filtered n))))
  (define (start)
    (if (filter a)
	a
	(next-filtered a)))
  (accumulate combiner null-value term (start) next-filtered b))

(define (sum-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b fast-prime2?))

(define (ex133b n)
  (define (filter x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) filter))

; 1.3
