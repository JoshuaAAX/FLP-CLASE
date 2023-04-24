;; <expresion> := <clausula> | (<clausula>) AND (<clausula>) | <expresion>
;; <clausula>:= <varible> | <variable> OR <variable> | <clausula>
;; <variable> := <number> | - <number>

;; punto 1.1

#lang eopl
         

(define digito_normal
   (lambda (n)
      n
   )
)

;;(digito_negado 5) -> -5
;;(digito_negado -5) -> 5

(define digito_negado
   (lambda (n)
      (* -1 n)
   )
)

;;(variable(digito_negado 4))-> -4
;;(variable(digito_normal 5))-> 5

(define digito?
  (lambda (digito)
    (number? digito)
  )
)

(define variable
  (lambda (variable)
    (if (digito? variable)
        variable
        'VariableNoConstruida
    )
  )
)


(define variable?
  (lambda (digit)
    (digito? digit )
  )
)


(define una_clausula
    (lambda (var)
      (if (variable? var)
           var
           'Una_clausulaNoConstruida
       )
    )
)

;;(una_clausula
;;        (variable(digito_normal 5))
;;       )
;; -> 5

;;(una_clausula
;;        (variable(digito_negado 4))
;;       )
;; -> -4

(define dos_clausulas
    (lambda (var1  var2)
      (if (and (variable? var1))
          (list 'or var1 var2)
           'dos_clausulasNoConstruida
      )
    )
)

;;(dos_clausulas
;;        (variable(digito_normal 5))
;;        (variable(digito_negado 4))
;;       ) -> (or 5 -4)

;; (dos_clausulas
;;        (variable(digito_normal 3))
;;        (variable(digito_normal 3))
;;       )
;; -> (or 3 3)

(define or->clausula
  (lambda (list)
    (car list)
  )
)

(define varlist->clausula
  (lambda (list)
    (cdr list)
  )
)

(define firstVar->clausula
  (lambda (list)
    (cadr list)
  )
)


(define clausula?
   (lambda (cla)
     (if (eq? (or->clausula cla) 'or)
         (if (pair? (varlist->clausula cla))
             (variable? (firstVar->clausula cla))
             #f
         )
         #f
     )
   )
)



(define una_expresion
    (lambda (cla)
      (if (clausula? cla)
           cla
           'Una_expresionNoConstruida
       )
    )
)

;;(una_expresion
;;       (dos_clausulas
;;        (variable(digito_normal 5))
;;        (variable(digito_normal 5))
;;       )
;;  )
;;  -> (or 5 5)

;;(una_expresion
;;       (dos_clausulas
;;        (variable(digito_negado 5))
;;        (variable(digito_negado 5))
;;       )
;;  )
;; -> (or -5 -5)

(define dos_expresiones
    (lambda (cla1  cla2)
      (if (and (clausula? cla1))
          (list 'and cla1 cla2)
           'dos_expresionesNoConstruida
      )
    )
)

;;(dos_expresiones
;;(dos_clausulas
;;       (variable(digito_normal 4))
;;       (variable(digito_normal 5))
;;      )
;;      (dos_clausulas
;;       (variable(digito_normal 6))
;;       (variable(digito_normal 7))
;;      )
;; )
;; ->(and (or 4 5) (or 6 7))

;;(dos_expresiones
;;(dos_clausulas
;;       (variable(digito_negado 4))
;;       (variable(digito_normal 5))
;;      )
;;      (dos_clausulas
;;       (variable(digito_normal 6))
;;       (variable(digito_negado 7))
;;      ))
;; -> (and (or -4 5) (or 6 -7))

(define and->expresion
  (lambda (list)
    (car list)
  )
)

(define varlist->expresion
  (lambda (list)
    (cdr list)
  )
)

(define firstCla->expresion
  (lambda (list)
    (cadr list)
  )
)

(define expresion?
   (lambda (exp)
     (if (eq? (and->expresion exp) 'and)
         (if (pair? (varlist->expresion exp))
             (variable? (firstCla->expresion exp))
             #f
         )
         #f
     )
   )
)


(define (combi-booleano x)
  (if (<= x 0)
      '(())
      (let ((combs (combi-booleano (- x 1))))
        (append (map (lambda (comb) (cons #t comb)) combs)
                (map (lambda (comb) (cons #f comb)) combs)))))


; (combi-booleano 2) => ((#t #t) (#t #f) (#f #t) (#f #f))
; (combi-booleano 3) => ((#t #t #t) (#t #t #f) (#t #f #t) (#t #f #f) (#f #t #t) (#f #t #f) (#f #f #t) (#f #f #f))

;;==================================================================================================================


;; <expresion> := <clausula> | (<clausula>) AND (<clausula>) | <expresion>
;; <clausula>:= <varible> | <variable> OR <variable> | <clausula>
;; <variable> := <number> | - <number>


;; punto 1.2
                  

#lang eopl


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


;;==================================================================================================================


;;unparse de datatypes


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
         (dos_clausulas (cla1  cla2) (list  (unparse-variable cla1) 'or (unparse-clausula cla2)))
     )
   )
)


(define unparse-expresion
   (lambda (exp)
     (cases expresion exp
         (una_expresion (exp) (unparse-clausula  exp))
         (dos_expresiones (exp1 exp2) (list  (unparse-clausula exp1) 'and (unparse-expresion exp2)))
     )
   )
)

(define unparse-bnf
   (lambda (instance-sat)
       (list 'fnc (unparse-expresion instance-sat))
   )
)






;;==================================================================================================================

;; unparse de  listas

;; punto 2.1

(define (ultimo lst)
  (cond
    ((null? lst) #f) 
    ((null? (cdr lst)) (car lst)) 
    (else (ultimo (cdr lst))) 
  )
)

(define (ultimo? elemento lista)
  (eq? elemento (ultimo lista)))


(define or-intermediate
  (lambda (lst)
    (cond
      ((null? lst) '()) 
      ((not (list? lst)) lst)
      ((eq? (car lst) 'or) (or-intermediate (cdr lst) ))
      (else (list 'or (car lst) (if (ultimo? (caddr lst) lst )
                                    (caddr lst)
                                    (or-intermediate (cdr lst))
                                  )
            )
      )
    )
  )
)


(define and-intermediate
  (lambda (lst)
    (cond
      ((null? lst) '()) 
      ((not (list? lst)) lst)
      ((eq? (car lst) 'and ) (and-intermediate (cdr lst) ))
      (else (list 'and (or-intermediate (car lst)) (if (ultimo? (caddr lst) lst )
                                    (or-intermediate (caddr lst))
                                    (and-intermediate (cdr lst))
                                  )
            )
      )
    )
  )
)



(define PARSERBNF
  (lambda (lst)
    (cond
      ((null? lst) '()) 
      ((not (list? lst)) lst)
      ((eq? (cadr lst) 'or ) (or-intermediate lst))
      (else (and-intermediate lst) )
    )
  )
)



