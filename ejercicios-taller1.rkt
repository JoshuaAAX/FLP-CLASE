

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
