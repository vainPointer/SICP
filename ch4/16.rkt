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

; b)
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
                (list set! (definition-variable first-clause) (definition-value first-clause)))
          (iter body
                (cons first-clause non-define-clauses)
                variables
                set-clauses)))))
  (let (body (lambda-body proc))
    (iter body '() '() '())))

