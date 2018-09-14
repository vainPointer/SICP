#lang sicp
; Ex 2.4
(define (mycons x y)
  (lambda (m) (m x y)))
(define (mycar z)
  (z (lambda (p q) p)))
(define (mycdr z)
  (z (lambda (p q) q)))
; Ex 2.4
(define (power a x)
  (define (iter acc n)
    (if (= x n) acc
        (iter (* acc a) (+ n 1))))
  (iter 1 0))
; Ex 2.5
(define (divides? a b)
  (= (remainder b a) 0))
(define (cons23 a b)
  (* (power 2 a) (power 3 b)))
(define (car23 c)
  (define (iter acc c)
    (if (divides? 2 c) (iter (+ acc 1) (/ c 2))
        acc))
  (iter 0 c))
(define (cdr23 c)
  (if (divides? 3 c)
      (+ 1 (cdr23 (/ c 3)))
      0))
; Ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) (lambda (x) (f x))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))
(define (plus first second)
  (lambda (f) (lambda (x) ((first f) ((second f) x)))))
(define (f x)
  (display "*"))
(define star_three
  (((plus one two) f) 'a))
; 具体化的f x
(define (inc x) (+ x 1))
(define nat_zero
  ((zero inc) 0))
(define nat_one
  ((one inc) 0))
(define nat_two
  ((two inc) 0))
; Ex 2.17
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (last-pair l)
  (if (= (length l) 1) (car l)
      (last-pair (cdr l))))
; Ex 2.18
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))
(define (reverse l)
  (if (null? l) nil
      (append (reverse (cdr l)) (list (car l)))))
; Ex 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr) 
(define first-denomination car)

(define (timed-cc amount coin-values start-time) 
  (cc amount coin-values) 
  (- (runtime) start-time)) 
(timed-cc 200 us-coins (runtime))       
(timed-cc 200 (reverse us-coins) (runtime)) 
  
(define (comp)
  (if (> (timed-cc 500 (reverse us-coins) (runtime)) 
         (timed-cc 500 us-coins (runtime))) 
      (display "Reverse takes longer") 
      (display "Reverse does not take longer"))) ; 逆序穷举情况变多，速度变慢。

; Ex 2.20
(define (same-parity x . rest)
  (define (same-parity-helper items)
    (cond ((empty? items) nil)
	  ((and (even? x) (even? (car items)))
	   (cons (car items) (same-parity-helper (cdr items))))
	  ((and (odd? x) (odd? (car items)))
	   (cons (car items) (same-parity-helper (cdr items))))
	  (else (same-parity-helper (cdr items)))))
  (cons x (same-parity-helper rest)))
(define (empty? l)
  (= (length l) 0))

; Ex 2.21
(define (square x) (* x x))
(define (map proc items)
  (if (null? items) nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list2 items)
  (map square items))

; Ex 2.23
(define (for-each f l)
  (cond ((not (null? l)) (f (car l)) (for-each f (cdr l)))))

; Ex 2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; Ex 2.27
(define (deep-reverse l)
  (cond ((null? l) nil)
        ((pair? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
        (else (append (cdr l) (list (car l))))))

; Ex 2.28
(define (fringe l)
  (cond ((null? l) nil)
        ((null? (car l)) (fringe (cdr l)))
        ((list? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

; Ex 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (balanced-branch? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))

(define (balanced? mobile)
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (and (= (* (branch-length lb) (branch-weight lb))
	    (* (branch-length rb) (branch-weight rb)))
	 (balanced-branch? lb)
	 (balanced-branch? rb))))

(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))
(define (right-branch2 mobile)
  (cdr mobile))
(define (branch-structure2 branch)
  (cdr branch))

; Ex 2.31
(define (tree-map2 proc tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map2 proc (car tree)) (tree-map2 proc (cdr tree))))
        (else (proc tree))))
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))                              ; 这种写法很厉害，学习
(define (square-tree tree)
  (tree-map square tree))

; Ex 2.32
(define (subsets s)                        ; let F(e,T)={X U {e}| x \in T}
  (if (null? s) (list nil)                 ; if S={}; P(S)={{}}
      (let ((rest (subsets (cdr s))))      ; otherwise, P(S) = P(T) U F(e,P(T)) ; P(T): rest ; (car s): e
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; Ex 2.33
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else cons (filter predicate (car sequence)) (filter predicate (cdr sequence)))))

(define (tree-filter predicate sequence)
  (cond ((null? sequence) nil)
        ((not (pair? (car sequence)))
         (if (predicate (car sequence))
             (cons (car sequence) (tree-filter predicate (cdr sequence)))
             nil))
        (else (cons
               (tree-filter predicate (car sequence))
               (tree-filter predicate (cdr sequence))))))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))   ; fold-right

(define (tree-acc op initial tree)
  (cond ((null? tree) initial)
        ((not (pair? (car tree)))
         (op (car tree) (tree-acc op initial (cdr tree))))
        (else (op (tree-acc op initial (car tree))
                  (tree-acc op initial (cdr tree))))))

; map append length 都可以看作 accumulate 操作
(define (map2 p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))
(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; Ex 2.34
(define (horner-eval x cof-seq)
  (accumulate (lambda (this-cof higher-terms)
                (+ this-cof (* higher-terms x)))
              0 cof-seq))

; Ex 2.35
(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (fringe t)))
(define (count-leaves3 t)
  (accumulate + 0
              (map (lambda (x) 1) (fringe t))))  ; accumulate <- map <- filter

; Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose mat)
  (accumulate-n (lambda (x y) (cons x y)) nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))


; Ex 2.38
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; Ex 2.39
(define (reverser sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))   ; fold-left ; 递归的
(define (reversel sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))                         ; 迭代的

; Ex 2.40
(define (range a b)
  (if (> a b) nil
      (cons a (range (+ a 1) b))))
(define (unique-pairs n)
  (map (lambda (x) (list n x)) (range 1 (- n 1))))

(define (flatmap proc seq) 
  (accumulate append nil (map proc seq))) ; proc 应用在一个数上返回一个 seq

; Ex 2.41-2.52
; nil

; Section 2.3
; Ex 2.53
