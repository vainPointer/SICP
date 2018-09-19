;; 基本过程
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))
(define (false? x) (eq? x false))           ; 除了false
(define (true? x) (not (eq? x false)))      ; 其他都是true

;; 基本过程安装进环境
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (define (primitive-implementation proc) (cadr proc))
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define apply-in-underlying-scheme apply) ; 这里为什么这样做?

;; 复合过程
(define (compound-procedure?   p) (tagged-list? p 'procedure))

(define (apply-compound-procedure procedure arguments)
  (define (procedure-parameters  p) (cadr p))
  (define (procedure-body        p) (caddr p))
  (define (procedure-environment p) (cadddr p))
  (eval-sequence                                        ; 顺序地求表达式值
   (procedure-body procedure)
   (extend-environment                                  ; 扩充该过程携带的基本环境
    (procedure-parameters  procedure)
    arguments
    (procedure-environment procedure))))

(define (make-procedure params body env)
  (list 'procedure params body env))
