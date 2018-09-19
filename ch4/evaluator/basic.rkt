(load "environment.rkt")  ; basic variables and primitive procedures
(load "keyword.rkt")      ; read -> eval -> (if keyword?) -> eval
(load "procedure.rkt")    ; (if application?) -> apply

;; 表达式求值 eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable?    exp) (lookup-variable-value exp env))   ; lookup-variable-value
        ((quoted?      exp) (text-of-quotation exp))
        ((assignment?  exp) (eval-assignment exp env))         ; set-variable!
        ((definition?  exp) (eval-definition exp env))         ; define-variable!
        ((if?          exp) (eval-if exp env))
        ((cond?        exp) (eval (cond->if exp) env))         ; cond 是 if 的派生
        ((lambda?      exp) (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                            env))
        ((let?         exp) (eval (let->lambda exp) env))      ; let 是 lambda 的派生
        ((begin?       exp) (eval-sequence (begin-actions exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

;; 函数过程应用 apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)                     
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure?  procedure)            
         (apply-compound-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))

;; main read-eval-print loop
(define (driver-loop)
  (define input-prompt  ";;; M-Eval input:")
  (define output-prompt ";;; M-Eval value:")
  (define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
  (define (announce-output string)
    (newline) (display string) (newline))
  (define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedre
                       (procedure-parameters object)
                       (procedure-body object)
                       '<procedure-env>))
        (display object)))                ; 这里没看懂？
  ;; 主体部分
  (prompt-for-input input-prompt)
  (let ((input (read)))                                   ; 读入
    (let ((output (eval input the-global-environment)))   ; 对读入求值
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))
(driver-loop)
