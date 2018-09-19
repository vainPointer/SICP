;; Louis的map会出错：
;; 求值器读入
;; '(map + '(1 2) '(3 4))
;; 调用

(apply (eval (operator exp) env)
       (list-of-values (operands exp) env)))

;; (eval (operator exp) env) 返回 scheme 自带的 map: '(primitive map)
;; 而 (list-of-values (operands exp) env))) 产生了自带 map 所不能使用的参数
;; + -> '(primitive +) 不被系统自带的 map 所接受