; a)
(define (lookup-variable-value var env)
  (scan-environment var env
    (lambda (vars vals) 
      (if (eq? (car vals) '*unassigned*)
          (error "Can't access '*unassigned* value " var)
          (car vals)))
    (lambda (frame)
      (lookup-variable-value var (enclosing-environment env)))
    (lambda ()
      (error "Unbound variable" var))))

; b) is copied from https://github.com/kana/sicp/blob/master/ex-4.16.scm
(define (scan-out-defines proc)
  (define (make-unassigned-binding variables)
    (map (lambda (var) (cons var '*unassigned*)) variables))
  (define (iter body non-define-clauses variables set-clauses)
    (if (null? body)
      (make-let (make-unassigned-binding variables)
                (append set-clauses non-define-clauses))
      (let ((first-clause (first-exp body)))
        (if (define? first-clause)
          (iter (rest-exps body)
                non-define-clauses
                (cons (definition-variable first-clause) variables)
                (list 'set! (definition-variable first-clause) (definition-value first-clause)))
          (iter body
                (cons first-clause non-define-clauses)
                variables
                set-clauses)))))
  (let (body (lambda-body proc))
    (iter body '() '() '())))

; c)
;;; Install `scan-out-defines` in the interpreter, either in
;;; `make-procedure` or in `procedure-body` (see section 4.1.3).
;;; Which place is better? Why?

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (procedure-body p)
  (scan-out-defines (caddr p)))

;; Each has its own merit:
;;
;; * In make-procedure: scan-out-defines is applied only once for each
;;   procedure.  It would improve performance of the metacircular evaluator,
;;   because procedure-body is expected to be applied for each application of
;;   compound procedures (in the metacircular evaluator), and scan-out-defines
;;   returns the equivalent result for each application with the same
;;   arguments.
;;
;; * In procedure-body: this keeps the original body.  If the metacircular
;;   evaluator found an error while applying a compound procedure, it would be
;;   useful for users to show where the error happens.  The original body must
;;   be kept to provide useful information to users.
