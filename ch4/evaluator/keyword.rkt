;; 关键字以其处理
;; 常量
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 变量
(define (variable? exp) (symbol? exp))

;; 其他关键字表示为带标签的list
(define (tagged-list? exp tag)                             ; quote set! define if cond lambda begin 都是带标签的list
  (if (pair? exp)
      (eq? (car exp) tag)
      false))  

;; quote
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; set!
(define (assignment? exp) (tagged-list? exp 'set!))
(define (eval-assignment exp env)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (set-variable-value! (assignment-variable exp)           ; 变量
                       (eval (assignment-value exp) env)   ; 得到的值
                       env)                                ; 放入环境
  'ok)

;; define
(define (definition? exp) (tagged-list? exp 'define))
(define (eval-definition exp env)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)                                           ; (define <var> <value>)
        (caadr exp)))                                        ; (define (<var> <args>) <body>)
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp) (cddr exp))))               ; (define <var> (lambda (<args>) <body>))
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (eval-if exp env)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (make-if predicate consequent alternative)           ; make-if服务于其派生cond
  (list 'if predicate consequent alternative))

;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp)
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-action clause) (cdr clause))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'flase
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))  ; 为什么一定要有这一句?cond-action不是本来就要求是一个begin吗?
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (expand-clauses (cond-clauses exp)))

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

;; let and named let
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-variables exp) (map car (cadr exp)))
(define (let-values exp) (map cdr (cadr exp)))
(define (let-body) (cddr exp))
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (named-let? exp) (and (let? exp) (eq? 4 (length exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-variables exp) (map car (caddr exp)))
(define (named-let-values exp) (map cdr (caddr exp)))
(define (named-let-body exp) (cadddr exp))

(define (let->lambda exp)
  (if (named-let? exp)
      ((list 'define (named-let-name exp)
             (make-lambda (named-let-variables exp)
                          (named-let-body exp)))
       ((named-let-name exp) (named-let-values exp)))
      ((make-lambda (let-variables exp) (let-body exp))
       (let-values exp))))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (eval-sequence exps env)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exp exps) env))))
(define (make-begin seq)
  (cons 'begin seq))

;; 函数过程 ; 如果以上都不是,那么就是函数过程 
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (list-of-values exps env)          ; 获取函数参数
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))