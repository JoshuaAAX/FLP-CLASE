#lang eopl

;;Joshua Chicame - 2074121
;;Damian Espinosa - 2028180
;; Taller 1 FLP

;; Ejercicio 1 /Append:
;; L1 L2 -> L
;; Proposito: Retorna una lista con todos los elementos de las listas de argumento dadas.
;; <lista> := ()
;;         := (<elemento> <lista>)


(define Append
  (lambda (l1 l2)
     (if(null? l1)
        l2
         (cons (car l1) (Append (cdr l1) l2)))))

;; Ejercicio 1 / aux-invert:
;; L -> L
;; Proposito: invierte los elementos de una lista
;; <lista> := ()
;;         := (<elemento> <lista>)


;; Ejercicio 1 / invert:
;; L -> L
;; Proposito: retorna la lista con sus pares ordenados invertidos  si cumplen con el predicado
;; <lista-par> := ()
;;             := ((<elemento> <elemento>) <lista-par>)
(define invert
 (lambda (L P)
    (if (null? L)
        L
        (if (and (P (caar L)) (P (cadar L)) )
            (cons (Append (cdar L) (cons (caar L) empty) ) (invert (cdr L) P))
            (invert (cdr L) P)
        )
     )
  )
)

;; Pruebas
 
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((6 9) (10 90) (82 7) ) odd? )

;; Ejercicio 2 / down:
;; L -> L
;; Proposito: retornar los elementos de la lista con un nivel más de paréntesis
;; <lista> := ()
;;     	:= ((<elemento> <lista>) <lista>)

(define down
  (lambda (L)
	(if (null? L)
    	L
   	(cons (cons (car L) empty) (down (cdr L)))
 	)
   )
)

;; PRUEBAS
(down '(1 2 3))
(down '((una) (buena) (idea)))
