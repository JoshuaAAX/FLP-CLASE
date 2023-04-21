;; <expresion> := <clausula> | <clausla> AND <clausula>
;; <clausula>:= <varible> | <variable> OR <variable>
;; <variable> := <digito> | NOT <digito>
;; <digito> := 1|2|...|n
                  

#lang eopl

(define digito
   (lambda (n)
      (list 'digito n)
   )
)

(define digito?
   (lambda (digito)
      (if (number? (cadr digito))
          #t
          #f
      )
   )
)

(define (negation-of-digito? exp)
  (and (eq? (car exp) 'NOT)
     #t
  )
)

(define variable?
  (lambda (variable)
    (or  (digito? variable)
        (negation-of-digito?  variable)
    )
  )
)


(define clausula
    (lambda (variable1  variable2)
      (list variable1'or variable2)
    )
)


(define expresion
    (lambda (clausula1  clausula2)
      (list  clausula1 'and clausula2)
    )
)


(define expresion? 
  (lambda (exp)
     (or (number? exp)
       (and (pair? exp)
           (eq? (cadr exp) 'AND)
           (number? (car exp))
           (expresion? (cddr exp))
        )
     )
  )
)

(define (combi-booleano x)
  (if (<= x 0)
      '(())
      (let ((combs (combi-booleano (- x 1))))
        (append (map (lambda (comb) (cons #t comb)) combs)
                (map (lambda (comb) (cons #f comb)) combs)))))

; Ejemplo de uso:
; (combi-booleano 2) => ((#t #t) (#t #f) (#f #t) (#f #f))
; (combi-booleano 3) => ((#t #t #t) (#t #t #f) (#t #f #t) (#t #f #f) (#f #t #t) (#f #t #f) (#f #f #t) (#f #f #f))
