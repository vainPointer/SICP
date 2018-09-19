(define (eval exp env)
  (let ((exp-method (get 'eval (exp-type exp))))
    (if exp-method
        (exp-method exp env)
        (apply (eval (operator exp) env) (list-of-values (operands exp) env)))))

(define (exp-type exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
        ((variable?        exp) 'variable)
        ((pair?            exp) (car exp))
        (else                   'unknown)))

(put 'eval 'self-evaluating (lambda (exp env) exp))
(put 'eval 'variable        lookup-variable-value)
(put 'eval 'quote           (lambda (exp env) (text-of-quatation exp)))
(put 'eval 'set!            eval-assignment)
(put 'eval 'define          eval-definition)
(put 'eval 'if              eval-if)
(put 'eval 'cond            (lambda (exp env) (eval (cond->if exp) env)))
(put 'eval 'lambda          (lambda (exp env)
                              (make-procedure
                               (lambda-parameters exp)
                               (lambda-body exp)
                               env)))
(put 'eval 'begin           (lambda (exp env)
                              (eval-sequence (begin-actions exp) env)))
(put 'eval 'unknown         (lambda (exp env)
                              (error "Unknown expression type -- EVAL" exp)))

