;; <expresion> := <clausula> | <clausla> AND <clausula>
;; <clausula>:= <varible> | <variable> OR <variable>
;; <variable> := <digito> | - <digito>
                  

#lang eopl

(define digito_normal
   (lambda (n)
      n
   )
)

(define digito_negado
   (lambda (n)
      (* -1 n)
   )
)

(define digito?
  (lambda (digito)
    (number? digito)
  )
)


(define variable?
  (lambda (variable)
    (digito? variable )
  )
)


(define clausula
    (lambda (variable1  variable2)
      (list 'or variable1 variable2)
    )
)


(define expresion
    (lambda (clausula1  clausula2)
      (list  'and clausula1  clausula2)
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

;;==================================================================================================================


;; <expresion> := <clausula> | (<clausula>) AND (<clausula>) | <expresion>
;; <clausula>:= <varible> | <variable> OR <variable> | <clausula>
;; <variable> := <number> | - <number>
;; 
                  

#lang eopl

(define-datatype bintree bintree?
  (empty-b-tree)
  (b-tree (num number?) (lson bintree?) (rson bintree?))
)

(define unparse
  (lambda (arb)
    (cases bintree arb
      (empty-b-tree() '())
      (b-tree (nodo left right)
              (list nodo (unparse left) (unparse right)))
      )
    )
  )


(define-datatype variable variable?
   (digito_normal  (digito number?) )
   (digito_negado  (digito number?) )
)


(define-datatype clausula clausula?
   (una_clausula (variable variable?) )
   (dos_clausulas (variable1 variable?) (variable2 clausula?) ) 
)



(define-datatype expresion expresion?
   (una_expresion (clausula clausula?))
   (dos_expresiones  (clausula1 clausula?) (clausula2 expresion?))
)



(define unparse-variable
   (lambda (var)
      (cases variable var
          (digito_normal (var) var )
          (digito_negado (var) (* -1 var))
       )
    )
)


(define unparse-clausula
   (lambda (cla)
     (cases clausula cla
         (una_clausula (cla) (unparse-variable  cla))
         (dos_clausulas (cla1  cla2) (list  (unparse-variable cla1) 'OR (unparse-clausula cla2)))
     )
   )
)


(define unparse-expresion
   (lambda (exp)
     (cases expresion exp
         (una_expresion (exp) (unparse-clausula  exp))
         (dos_expresiones (exp1 exp2) (list  (unparse-clausula exp1) 'AND (unparse-expresion exp2)))
     )
   )
)




