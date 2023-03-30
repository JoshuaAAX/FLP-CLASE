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

;; Ejercicio 3 / list-set:
;; L n x -> L
;; Proposito: retorna la lista pero teniendo en la posición n el elemento x solo si cumple el predicado
;; <lista> := ()
;;         := ((<elemento> <lista>) <lista>)

(define list-set
  (lambda (L n x P)
    (aux-list-set L n x 0 P)
  )
)

;; PRUEBAS
(list-set '(1 2 3 4) 2 '(1 2) even?)
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)
(list-set '(5 8 7 6) 3 '(1 5 10) mayor5? )
(list-set '(5 8 7 6) 0 '(1 5 10) mayor5? )


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

;;==============================================================================================

;; Ejercicio 5 / aux-list-index:
;; L P counter -> L
;; Proposito: retornar la posición del primer elemento que satisface el predicado,
;;        	en caso de no hacerlo retorna #f guardando la posición en counter
;; <lista> := ()
;;     	:= ((<elemento> <lista>) <lista>)

(define aux-list-index
  (lambda (P L counter)
	(if (null? L)
    	#f
    	(if (P (car L))
         	counter
         	(aux-list-index P (cdr L) (+ counter 1))
     	)    
 	)
   )
)

;; Ejercicio 5 / list-index:
;; L P -> L
;; Proposito: retornar la posición del primer elemento que satisface el predicado,
;;        	en caso de no hacerlo retorna #f
;; <lista> := ()
;;     	:= ((<elemento> <lista>)<lista>)

(define list-index
  (lambda (P L)
 	(aux-list-index P L 0)
   )
)
;; PRUEBAS
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(1 2 (a b) 3))

;;==============================================================================================

;; Ejercicio 6 / swapper
;; L E1 E2 -> L
;; Proposito: La función retorna una lista similar a L,
;;        	solo que cada ocurrencia anterior de E1 será reemplazada por E2
;;        	y cada ocurrencia anterior de E2 será reemplazada por E1
;; <lista> := ()
;;     	:= (<elemento> <lista>)

(define swapper
  (lambda (E1 E2  L)
 	(if (null? L)
      	L
      	(cond
         	[(equal? (car L) E1)
          	(cons E2 (swapper E1 E2 (cdr L)))]
         	[(equal? (car L) E2)
          	(cons E1 (swapper E1 E2 (cdr L)))]
         	[else (cons (car L) (swapper E1 E2 (cdr L)))]     	 
       	)
  	)
   )
)

;; PRUEBAS
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))



;;==============================================================================================

;; Ejercicio 7 / aux-product:
;; L E1 -> L
;; Proposito: combinar el elemento e1 con cada elemento de la L
;; <lista> := ()
;;         := (<elemento> <lista>)
(define aux-product
  (lambda (E1 L)
    (if(null? L)
        L
        (cons  (cons E1 (cons (car L) empty)) (aux-product E1 (cdr L)))
     )
  )
)

;; Ejercicio 7 / cartesian-product:
;; L E1 -> L
;; Proposito: retornar la tuplas que representan el producto cartesiano de las dos listas
;; <lista> := ()
;;         := (<elemento> <lista>)
(define cartesian-product
  (lambda (L1 L2)
    (if (null? L1)
        L1
        (append (aux-product (car L1) L2) (cartesian-product (cdr L1) L2) )
    )
  )
)

; PRUEBAS
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))

;;==============================================================================================

;; Ejercicio 8 / mapping
;; F L1 L2 -> L
;; Propósito: La funcíon debe retornar una lista de pares (a,b) siendo a elemento de L1 y b elemento de L2
;;cumpliendose la propiedad que al aplicar la funcíon unaria F con el argumento  tal que F(a) = b.


(define mapping
  (lambda (F L1 L2)
    (cond [(null? L1) empty]
          [(null? L2) empty]
          [(= (car L2) (F (car L1)))
           (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2)))]
          [else (mapping F (cdr L1) (cdr L2))]
          )
    ))
;;PRUEBAS
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))


;;==============================================================================================

;; Ejercicio 9 / aux-inversions:
;; x L -> INT
;; Proposito: Retorna cuantas posiciones avanzo para acomodar el elemento x en la lista L
;;
;;<lista> := ()
;;        := (<elemento-número> <lista>)
(define aux-inversions
  (lambda (x L counter)
     (if (null? L)
         counter
        (if (< (car L) x)
            (aux-inversions x (cdr L) (+ counter 1))
            (aux-inversions x (cdr L) counter)
         )
      )
   )
)

;; Ejercicio 9 / inversions:
;; L -> INT
;; Proposito: determina el n´umero de inversiones de la lista L. De manera formal, sea A = (a1a2...an) una lista de n n´umeros diferentes, si i < j (posicion)
;;y ai > aj (dato en la posici´on) entonces la pareja (i j) es una inversi´on de A.
;;
;;<lista> := ()
;;        := (<elemento-número> <lista>)
(define inversions
  (lambda (L)
     (if (null? L)
          0
          (+ (aux-inversions (car L) (cdr L) 0) (inversions (cdr L)))
      )
   )
)

;; PRUEBAS
(inversions '(2 3 8 6 1)) 
(inversions '(1 2 3 4))

;;==============================================================================================

;; Ejercicio 10 / aux-up:
;; L -> L
;; Proposito: retorna elementos con un nivel menos de parentesis
;; <lista> := ()
;;         := (<elemento> <lista>)

(define aux-up
  (lambda (hlist rlist)
   (cond
     [(null? hlist)
     (up rlist)]
     [(pair? hlist)
      (cons (car hlist)
           (aux-up (cdr hlist) rlist))]
      [else
       (cons hlist (up rlist))])))

;; Ejercicio 10 / up:
;; L -> L
;; Proposito: Retorna lista con un un nivel menos de parentesis.
;; <lista> := ()
;;         := (<elemento> <lista>)

(define up
   (lambda (L)
  (if (null? L)
      '()
      (aux-up (car L) (cdr L)))))


;; PRUEBAS
(up '((1 2) (3 4)))
(up '((x (y)) z))

;;==============================================================================================

;;Ejercicio 11 / zip
;;F L L2 -> L
;; Proposito: Retorna una lista donde la posición n-ésima es el resultado de aplicar F sobre los elementos de L1 yL2 en esa posición.
;; <lista> := ()
;;         := (<int> <lista>)
(define zip
  (lambda (F L1 L2)
     (if (eqv? L1 '())
      empty   
      (cons (F(car L1) (car L2))
            (zip F (cdr L1) (cdr L2))))))
            
;; PRUEBAS
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))





;;==============================================================================================



;; Ejercicio 12 / filter-acum
;; a, b, F, acum, filter -> int: Recibe 5 entradas, las dos primeras corresponden a los valores
;; Proposito: 
;; Filter acum recibe cinco parametros, dos numeros a y b una funcion binaria F, un valor inicial acum y una funcion unaria filter
;; y retorna todos los elemento que estan en el intervalo [a,b] y que a su vez cumplen con el predicado de la funcion filter
;; el resultado debe ir consevando en acum y finalmente retornara el ultimo valor obtetenido en acum.

(define filter-acum
  (lambda(a b F acum filter)
    (if (and (eqv? F *) (eqv? acum 0))
       (if(<= a b)
   (if (filter a) (F 1 (filter-acum (+ a 1) b F a filter))
       (filter-acum (+ a 1) b F 1 filter))
     acum)
        
        (cond
          [(<= a b)(cond
                     [(filter a) (F acum (filter-acum (+ a 1) b F a filter))]
                     [else (filter-acum (+ a 1) b F acum filter)])]
          [else acum])
        )
    )
  )


;;PRUEBAS
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)

;;==============================================================================================
;;Ejercicio 13 / operate
;; L , L -> INT
;;Proposito: La funcion retorna el resultado de aplicar
;; sucesivamente las operaciones en lrators ( lista de funciones binarias de tama˜no n ) a los valores en lrands(a lista
;;de numeros de tama˜no n + 1).

(define operate
   (lambda (lrators lrands)
       (if (null? lrators)
           (car lrands)
           (operate (cdr lrators) (cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands) ))
       )
   )
)

;;Pruebas

(operate (list + * + - ) '(1 2 8 4 11 6))
(operate (list *) '(4 5))

;;==============================================================================================



;; Ejercicio 14
; path: int list -> list
; Proposito: procedimiento que busca el camino del elemento n dentro del arbol binario de busqueda BST
(define path
  (lambda (n BST)
    (cond
      [(equal? n (car BST)) empty]
      [(> n (car BST))(cons 'right (path n (caddr BST)))]
      [(< n (car BST))(cons 'left (path n (cadr BST)))]
      )
    )
  )

;;Pruebas
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))


;;==============================================================================================

;; Ejercicio 16 / simpson-rule
;;  Función auxiliar de sumatoria


(define (aux-sum term a next b)
      (if (> a b)
          0
          (+ (term a)
             (aux-sum term (next a) next b))))
;; simpson-rule
;; f a b n -> int
;; Proposito: c ́alcula la integral de una funci ́on f entre los valores a y b mediante la regla de Simpson.
;; <lista> := <int>


    (define (simpson-rule f a b n)
      (define h (/ (- b a) n))
      (define (next x) (+ x (* 2 h)))
      (* (/ h 3) (+ (f a)
                    (* 4 (aux-sum f (+ a h) next (- b h)))
                    (* 2 (aux-sum f (+ a (* 2 h)) next (- b (* 2 h))))
                    (f b))))



;;PRUEBAS
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)

