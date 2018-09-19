;; 环境定义
(define the-empty-environment '())                 ; env: (frames)
(define (first-frame env) (car env))               ; 当前环境
(define (enclosing-environment env) (cdr env))     ; 外部环境

(define (frame-variables frame) (car frame))       ; 变量名
(define (frame-values    frame) (cdr frame))       ; 变量值
(define (make-frame variables values)              ; frame: ((vars)(vals))
  (cons variables values))

;; 基本操作:扩展环境
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied"  vars vals))))

;; 基本过程:扫描环境
(define (scan-environment var env found not-found-in-frame not-found-in-env)
  (define frame (first-frame env))
  (if (eq? env the-empty-environment)
    (not-found-in-env)                        ; 在 env 中没有找到
    (let go ((vars (frame-variables frame))
             (vals (frame-values frame)))
      (cond ((null? vars)
             (not-found-in-frame frame))      ; 在 frame 中没有找到
            ((eq? var (car vars))
             (found vars vals))               ; 找到时，用 found 函数操作
            (else
              (go (cdr vars) (cdr vals)))))))

;; 查找环境变量
(define (lookup-variable-value var env)
  (scan-environment var env
    (lambda (vars vals) 
      (if (eq? (car vals) '*unassigned*)      ; Ex 4.16 a)
          (error "Can't access '*unassigned* value " var)
          (car vals)))
    (lambda (frame)
      (lookup-variable-value var (enclosing-environment env)))
    (lambda ()
      (error "Unbound variable" var))))

;; 赋值环境变量
(define (set-variable-value! var val env)
  (scan-environment var env
    (lambda (vars vals)
      (set-car! vals val))
    (lambda (frame)
      (set-variable-value! var val (enclosing-environment env)))
    (lambda ()
      (error "Unbound variable -- SET!" var))))

;; 定义环境变量
(define (define-variable! var val env)
  (scan-environment var env
    (lambda (vars vals)
      (set-car! vals val))
    (lambda (frame)
      (add-binding-to-frame! var val frame))
    #f))

;; 初始化安装环境
(define (setup-environment)
  (let ((initial-env
        (extend-environment (primitive-procedure-names)   ; 'car
                            (primitive-procedure-objects) ; (list 'primitive car)
                            the-empty-environment)))      ; 将基本过程安装进环境变量
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
