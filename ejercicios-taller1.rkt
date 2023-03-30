#lang eopl

;;Joshua Chicame - 2074121
;;Damian Espinosa - 2028180
;; Taller 1 FLP

;;==============================================================================================

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

;;==============================================================================================

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

;;==============================================================================================

;; Ejercicio 3 / aux-list-set:
;; L n x counter-> L
;; Proposito: retorna la lista pero teniendo en la posición n el elemento x solo si cumple el predicado
;;            mediante un contador que lleva las posiciones
;; <lista> := ()
;;         := ((<elemento> <lista>) <lista>)

(define aux-list-set
  (lambda (L n x counter P)
    (if (null? L)
        L
        (if (and (= counter n) (P (car L)) )
            (cons x (aux-list-set (cdr L) n x (+ counter 1) P))
            (cons (car L) (aux-list-set (cdr L) n x (+ counter 1) P))
         )
     )
   )
)

;; mayor5?
;; int -> boolean
;; Proposito: retorna true si el número es 5 en caso contrario retorna false
;; <elemento> := ()
;;            := <número> 
(define mayor5?
   (lambda (n)
     (if (> n 5)
       #t
       #f
     )
   )
)


;;PRUEBA
(aux-list-set '(1 2 3 4) 2 '(1 2) 0 even?)
(aux-list-set '(5 8 7 6) 2 '(1 2) 0 odd?)
(aux-list-set '(5 8 7 6) 2 '(1 2) 0 even?)
(aux-list-set '(5 8 7 6) 3 '(1 5 10) 0 mayor5? )
(aux-list-set '(5 8 7 6) 0 '(1 5 10) 0 mayor5? )


;;==============================================================================================


;; Ejercicio 4 / filter-in:
;; L P -> L
;; Proposito: retorna una lista con los elementos que stisfacen el predicado
;; <lista> := ()
;;     	:= ((<elemento> <lista>) <lista>)

(define filter-in
  (lambda (P L)
	(if (null? L)
    	L
    	(if (P (car L))
        	(cons (car L) (filter-in P (cdr L)))
        	(filter-in P (cdr L))
     	)
 	)
   )
)

;; PRUEBAS
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))


