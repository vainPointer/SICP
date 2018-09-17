;; Section 3.1
;; Ex 3.1
(define (make-accumulator init)
  (lambda (addend)
    (begin (set! init (+ init addend))
	   init)))
;; Ex 3.2
(define (make-monitored proc)
  (define calls 0)
  (define (mf m)
      (cond ((eq? m 'how-many-calls?) calls)
  	    ((eq? m 'reset-count) (set! calls 0))
	    (else (begin (set! calls (+ 1 calls))
			 (proc m)))))
  mf)
;; Ex 3.8
(define (make-f)
  (let ((seed 2))
    (lambda (x)
      (set! seed (- seed 1))
      (* x seed))))
(define f (make-f))
;; f1, f0 使用了同一块内存 seed
(define f1 (f 1))
(define f0 (f 0))

;; 串行化的实现
(define (make-serializer)
  (let ((mutex (make-mutex)))  ;; mutex 互斥元，可以被获取（acquired）或者释放（released）; 当被获取，对于这一互斥元的其他获取操作都必须等到该互斥元被释放之后
    (lambda (p)                ;; 输入一个过程p，返回一个（获取相应互斥元，运行p，释放互斥元的）过程
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      seralized-p)))

(define (make-mutex)           ;; 互斥元构造函数
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire))) ; retry
	    ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car cell false))

(define (test-and-set! cell)  ;; 必须以原子操作的方式执行
  (without-interrupts
   (lambda ()
     (if (car cell)
	 true
	 (begin (set-car! cell true)
		false)))))
  
