#lang eopl

;******************************************************************************************
;;;;; Interpretador Mini Py

;; repositorio: https://github.com/JoshuaAAX/FLP-CLASE/

;;Joshua Chicame 2074121
;;Damian Espinosa 2028180
;;Luisa Cardenas 1823494

;;*********************************Gramatica**************************************

;;<BSAT>            ::= {<class-decl>}* <expresion>
;;                      <bsat-program (class exp)>
;;<class-decl>      ::= class <identificador> extends <identificador>
;;                      {field <identificador>}* {<method-decl>}*
;;                      <a-class-decl(name super fields-id methods)>
;;<method-decl>     ::= def <identificador> ({<identificador>}(,)) <expresion>
;;                      <a-method-decl (name ids body)>
;;<expresion>       ::= <numero>
;;                      <num-exp (datum)>
;;                  ::= x16( {<numero>}* )
;;                      <numerohex-exp (lsnum)>
;;                  ::= '<caracter>'
;;                      <caracter-exp (caracter)>
;;                  ::= "<cadena>"
;;                      <cadena-exp (cadena)>
;;                  ::= <identificador>
;;                      <identificador-exp (id)>
;;                  ::= $<identificador>
;;                      <refid-exp (id)>
;;                  ::= var {<identificador> = <expresion>}*(,) in <expresion>
;;                      <var-exp (ids exps cuerpo)>
;;                  ::= set <identificador> = <expresion>
;;                      <set-exp (id exp)>
;;                  ::= cons {<identificador> = <expresion>}*(,)
;;                      <cons-exp (ids exps cuerpo)> in <expresion>
;;                  ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <rec-exp (lproc ids cuerpos cuerporec)>
;;                  ::= <lista>
;;                      <lista-exp (lista)>
;;                  ::= <vectorB>
;;                      <vector-exp (vector)>
;;                  ::= vector?(<expresion>)
;;                      <isvector-exp exp>
;;                  ::= <registro>
;;                      <registro-exp (registro)>
;;                  ::= register?(<expresion>)
;;                      <registros?-exp (exp)>
;;                  ::= <exp-bool>
;;                      <bool-exp (exp-bool)>
;;                  ::= begin {<expresion>}+(;) end
;;                      <begin-exp (exp lexps)>
;;                  ::= if <expr-bool> then <expresion> else <expresion> end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do <expresion> done
;;                      <while-exp (expb exp)>
;;                  ::= for <identificador> = <expresion> <to-odownto> <expresion> do <expresion> done
;;                      <for-exp (id exp1 to-odwto exp2 exp3)>
;;                  ::= create-reg(<identificador> = <expresion> , <expresion>)
;;                      <create-reg (id exp reg)>
;;                  ::= set-vec ( <expresion> , <expresion> , <expresion>)
;;                      <set-vec (pos vec val)>
;;                  ::= set-reg ( <expresion> , <expresion> , <expresion>)
;;                      <set-reg (pos reg val)>
;;                  ::= ref-reg(<identificador>,<registro>)
;;                      <ref-reg-exp (id, reg)>
;;                  ::= <prim-bin> (expresion , expresion)
;;                      <primbin-exp (lexp)>
;;                  ::= <prim-un> (expresion)
;;                      <primun-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {expression}*)
;;                      <app-exp (expresion lexps)>
;;                  ::= print (<expresion>)
;;                      <print-exp>
;;                  ::= FNC <numero> (<clausula-or>)+("and")
;;                      <fnc-exp (numero cla-or lcla-or)>
;;                  ::= new <identificador> ({expresion}*(,))
;;                      <new-object-exp(class-name rands)>
;;                  ::= send <expresion> <identificador> ({<expresion>}*(,))
;;                  ::= <method-app-exp(obj-exp method-name rands)>
;;                  ::= super <identificador> ({<expresion>}*(,))
;;                      <super-call-exp(method-name rands)>
;;<clausula-or>     ::= (<numero>)+("or")
;;                      <clausula-or-exp (n lsn)>
;;<prim-un>         ::= solveFNC | lenght
;;                  ::= add1 | sub1 | add1_16 | sub1_16
;;                  ::= list? | head | tail
;;<prim-bin>        ::= + | - | * | % | / | +_16 | -_16 | *_16
;;                  ::= create-list | append | create-vec | create-reg
;;                  ::= ref-vec | set-reg | concat
;;<lista>           ::= empty
;;                      <empty-list>
;;                  ::= [{<expresion>}*(;)]
;;                      <lista1 (lexps)>
;;<vectorB>          ::= vector[{<expresion>}*(;)]
;;                      <vector1 (lexps)>
;;<registro>        ::= {{<identificador> = <expresion>}+(;)}
;;                      <registro1 (id exp lids lexps)>
;;<expr-bool>       ::= <pred-prim> (<expresion> , <expresion>)
;;                      <comparacion (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<expr-bool> , <expr-bool>)
;;                      <conjuncion (obbool expb1 expb1)>
;;                  ::= <bool>
;;                      <vlr-bool (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <op-comp (oubool expb)>
;;<to-odownto>      ::= to
;;                      <to>
;;                  ::= downto
;;                      <downto>
;;<pred-prim>       ::= <|>|<=|>=|==|!=
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
;;<bool>            ::= true | false


;;==================================================================================

;;Definición Lexico

(define lexico
  '(
    (espacioblanco (whitespace) skip)
    (comentario ("#" (not #\newline)) skip)
    (identificador ("@" letter (arbno (or letter digit))) symbol)
    (letras (letter) string)
    (letras (letter (arbno (or letter digit))) string)
    (numero (digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

;;===================================================================================

;;Definición Grámatica
(define gramatica
  '(
    (BSAT ((arbno class-decl)expresion) bsat-program)
    (class-decl
     ("class" identificador "extends" identificador
              (arbno "field" identificador) (arbno method-decl)) a-class-decl)
    (method-decl ("def" identificador "(" (separated-list identificador ",") ")" expresion) a-method-decl)
    (expresion (numero) num-exp)
    (expresion ("x_16(" (arbno numero) ")") numerohex-exp)
    (expresion ("'" letras "'") caracter-exp)
    (expresion ("\"" letras "\"") cadena-exp)
    (expresion (identificador) identificador-exp)
    (expresion ("$" identificador) refid-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion)  var-exp)
    (expresion ("set" identificador "=" expresion) asignar-exp)
    (expresion ("cons" (separated-list identificador "=" expresion ",") "in" expresion)  cons-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion)
                rec-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("for" identificador "=" expresion to-o-downto expresion "do" expresion "done") for-exp)
    (expresion (prim-bin "(" expresion "," expresion ")") primbin-exp)
    (expresion (prim-un "(" expresion ")") primun-exp)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion) proc-exp)
    (expresion ("(" expresion (arbno expresion) ")") app-exp)
    (expresion ("print" "(" expresion ")") print-exp)
    (expresion ("FNC" numero "(" clausula-or (arbno "and" clausula-or) ")") fnc-exp)
    (expresion ("if" expr-bool "then" expresion "else" expresion "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("set-vec" "(" expresion "," expresion "," expresion ")") set-vec-exp)
    (expresion ("set-reg" "(" expresion "," expresion "," expresion ")") set-reg-exp)
    (expresion ("ref-reg" "(" identificador "," registro ")") ref-reg-exp)
    (expresion ("create-reg" "(" identificador "=" expresion "," expresion")") crear-reg-exp)
    (expresion (lista) lista-exp)
    (expresion (vectorB) vector-exp)
    (expresion (registro) registro-exp)
    (expresion (expr-bool) bool-exp)
    (expresion ("register?" "(" expresion ")") registros?-exp)
    (expresion ("vector?" "(" expresion ")") isvector-exp)
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)
    (expresion ("send" expresion identificador "(" (separated-list expresion ",") ")") method-app-exp)
    (expresion ("super" identificador "(" ( separated-list expresion ",") ")" ) super-call-exp)
    (lista ("empty") empty-list)
    (lista ("[" (separated-list expresion ",") "]") lista1)
    (vectorB ("vector" "[" (separated-list expresion ",") "]") vector1)
    (registro ("{"(separated-list identificador "=" expresion ";")"}") registro1)
    (expr-bool (pred-prim "(" expresion "," expresion ")") comparacion)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") conjuncion)
    (expr-bool (bool) vlr-bool)
    (expr-bool (oper-un-bool "(" expr-bool ")") op-comp)
    (clausula-or ("(" numero (arbno "or" numero) ")" ) clausula-or-exp)
    (to-o-downto ("to") to)
    (to-o-downto ("downto") downto)
    (bool ("true") true-exp)
    (bool ("false") false-exp)
    
    
    ;;===================================================================================

;;Primitivas unarias
    (prim-un ("solveFNC") solve-fnc)
    (prim-un ("add1") add1)
    (prim-un ("sub1") sub1)
    (prim-un ("add1_16") add1_16)
    (prim-un ("sub1_16") sub1_16)
    (prim-un ("lenght") lenght-exp)
    (prim-un ("list?") lista?-exp)
    (prim-un ("head") cabeza-exp)
    (prim-un ("tail") cola-exp)

   ;;===================================================================================

;;Primitivas binarias
    (prim-bin ("%") moduloB)
    (prim-bin ("+") suma)
    (prim-bin ("-") resta)
    (prim-bin ("*") mult)
    (prim-bin ("/") division)
    (prim-bin ("+_16") suma16)
    (prim-bin ("-_16") resta16)
    (prim-bin ("*_16") mult16)
    (prim-bin ("concat") concat-exp)
    (prim-bin ("append") append-exp)
    (prim-bin ("create-list") crear-lista-exp)
    (prim-bin ("create-vec") crear-v-exp)
    (prim-bin ("ref-vec") ref-vec-exp)

    (pred-prim ("<") menor-exp)
    (pred-prim (">") mayor-exp)
    (pred-prim ("<=") menor=exp)
    (pred-prim (">=") mayor=exp)
    (pred-prim ("==") igual=exp)
    (pred-prim ("!=") diferente-exp)
    (oper-bin-bool ("and") and-exp)
    (oper-bin-bool ("or") or-exp)
    (oper-un-bool ("not") not-exp)
    )
  )
  
;;===================================================================================
(sllgen:make-define-datatypes lexico gramatica)
;(sllgen:list-define-datatypes lexico gramatica)
;;===================================================================================
;;scan&parse

(define scan&parse
  (sllgen:make-string-parser lexico gramatica))

;;===================================================================================
;;ambiente

(define-datatype ambiente ambiente?
  (empty-env)
  (extend-env (lvar (list-of symbol?))
              (lvalor vector?)
              (env ambiente?)
              )
  )

(define recursively-extended-env-record
  (lambda (proc-names lidss bodies old-env)
    (let*
        (
         (len (length proc-names))
         (vec (make-vector len))
         (env (extend-env proc-names vec old-env))
         )
      (letrec
          [
           (actualizar-vector
            (lambda (pos lidds lbodies)
              (cond
                [(null? lidds) env]
                [else
                 (begin
                   (vector-set! vec pos (direct-target (closure (car lidds) (car lbodies) env)))
                   (actualizar-vector (+ pos  1) (cdr lidds) (cdr lbodies))
                   )
                 ]
                )
              )
            )
           ]
        (actualizar-vector 0 lidss bodies)
       )
        )
      )
  )

;;===================================================================================
;;referencia
(define-datatype referencia referencia?
  (a-ref (pos number?) (vec vector?))
  )

;;===================================================================================
;;validar target de referencia

(define ref-to-direct-target?
  (lambda (val)
    (if (referencia? val)
        (cases referencia val
          (a-ref (pos vec)
                 (cases target (vector-ref vec pos)
                   (direct-target (expval) #t)
                   (cons-target (expval) #t)
                   (indirect-target (ref) (eopl:error "no puede pasar una referencia enre procedimientos")
                   )
                 )
                 )
          )
        #f
        )
    )
  )

;;===================================================================================
;;Validar Target directo

(define no-refid-exp?
  (lambda (exp)
    (cond
      [(number? exp) #t]
      [(expresion? exp)
       (cases expresion exp
         (refid-exp (id) #f)
         (else #t)
         )]
      [else #t]
      )
    )
  )

;;===================================================================================
;;Target
(define-datatype target target?
  (direct-target (expval no-refid-exp?))
  (indirect-target (ref ref-to-direct-target?))
  (cons-target (expval no-refid-exp?))
  )
;;===================================================================================
;;Bignum

(define zero
  (lambda ()
    '()))

(define base 16)

(define is-zero?
  (lambda (n)
    (null? n)))

(define successor
  (lambda (n)
    (if (is-zero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t base)
	      (cons 0 (successor (cdr n)))
	      (cons t (cdr n))
              )
          )
        )
    )
  )

(define predecessor
  (lambda (n)
    (cond
     ((is-zero? n) (eopl:error "cero no tiene predecesor"))
     ((>= (car n) base) (eopl:error "el valor debe ser menor que 16"))
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  (eopl:error "cero no tiene predecesor")
	  (cons (- base 1) (predecessor (cdr n)))
          )
      )
      (else (cons (- (car n) 1) (cdr n)))
      )
    )
  )

(define suma-bignum
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma-bignum (predecessor x) y)))))

(define resta-bignum
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta-bignum  x (predecessor y))))))

(define mult-bignum
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma-bignum (mult-bignum (predecessor x) y) y))
    ))
    
;;===================================================================================
;;apply-env-ref

(define apply-env-ref
  (lambda (amb var)
    (cases ambiente amb
      (empty-env () (eopl:error "no se encontró la variable ~s" var))
      (extend-env (lvar vec env)
                  (letrec
                      [
                       (buscar-ambiente
                        (lambda (pos lids)
                          (cond
                            [(null? lids) (apply-env-ref env var)]
                            [(equal? var (car lids)) (a-ref pos vec)]
                            [else (buscar-ambiente (+ pos 1) (cdr lids))]
                            )
                          )
                        )
                       ]
                    (buscar-ambiente 0 lvar)
                      )
                  )
      )
    )
  )

;;===================================================================================
;;def-ref

(define def-ref
  (lambda (ref)
        (cases target (primitive-deref ref)
          (direct-target (exp-val) exp-val)
          (cons-target (exp-val) exp-val)
          (indirect-target (ref1)
                           (cases target (primitive-deref ref1)
                             (direct-target (exp-val) exp-val)
                             (cons-target (exp-val) exp-val)
                             (indirect-target (ref2)
                                              (eopl:error "solo se pueden de 1 referencia a otra")
                                              )
                             )
                           )
          )
    )
  )

;;===================================================================================
;;primitive-deref

(define primitive-deref
 (lambda (ref)
   (cases referencia ref
     (a-ref (pos vec)
            (vector-ref vec pos)
            )
     )
   )
 )

;;===================================================================================
;;set-ref!

(define set-ref!
  (lambda (ref val)
    (let
        ((ref
          (cases target (primitive-deref ref)
            (direct-target (exp-val) ref)
            (cons-target (exp-val) (eopl:error "No se puede cambiar el valor de una constante"))
            (indirect-target (ref1) ref1)
            )
          )
         )
      (primitive-setref! ref (direct-target val))
      )
    )
  )

;;===================================================================================
;;primitive-setref!

(define primitive-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec)
             (vector-set! vec pos val)
             )
      )
    )
  )

;;===================================================================================
;;apply-env

(define apply-env
  (lambda (env var)
    (def-ref (apply-env-ref env var))
   )
  )
  

;;===================================================================================
;;Ambiente inicial
(define init-env
  (extend-env (list '@x '@y '@z '@a)
              (list->vector (list (direct-target 4)
                                  (direct-target 2)
                                  (direct-target 5)
                                  (indirect-target (a-ref 0 (list->vector (list (direct-target 4)
                                  (direct-target 2)
                                  (direct-target 5)))))))
              (empty-env))
  )

;;===================================================================================
;eval-binprim

(define eval-binprim
  (lambda (op op1 op2)
    (cases prim-bin op
      (suma () (+ op1 op2))
      (resta () (- op1 op2))
      (moduloB () (modulo op1 op2))
      (mult () (* op1 op2))
      (division () (/ op1 op2))
      (suma16 () (suma-bignum op1 op2))
      (resta16 () (resta-bignum op1 op2))
      (mult16 () (mult-bignum op1 op2))
      (concat-exp () (string-append op1 op2))
      (append-exp () (append op1 op2))
      (crear-lista-exp () (append op2 (list op1)))
      (crear-v-exp () (list->vector (append (vector->list op2) (list op1))))
      (ref-vec-exp () (vector-ref op2 op1))
      )
    )
  )

;;===================================================================================
;eval-clausulaor

(define eval-clausulaor
  (lambda (cla)
    (cases clausula-or cla
      (clausula-or-exp (n lnum)
                       (append (list n) lnum)
                       )
      )
    )
  )
 
;;===================================================================================
;eval-fnc

(define eval-fnc
  (lambda (n claor lclaor)
    (list n (append (list (eval-clausulaor claor)) (map eval-clausulaor lclaor)))
    ))
    
;;===================================================================================

;nextcom

(define nextcom
  (lambda (n)
    (if (is-zero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t 2)
	      (cons 0 (nextcom (cdr n)))
	      (cons t (cdr n))
              )
          )
        )
    )
  )
;;===================================================================================
;convert-num->bool

(define convert-num->bool
  (lambda (x)
    (if (zero? x)
        #f
        #t
        )
    )
  )

;;===================================================================================
;convert-list->vector

(define convert-list->vector
  (lambda (lst)
    (cond
      [(null? lst) empty]
      [else
       (cons (list->vector (map convert-num->bool (car lst))) (convert-list->vector (cdr lst)))]
      )
    )
  )
  
 
;;===================================================================================

;combinaciones

(define combinaciones
  (lambda (n)
    (letrec
        [
         (limit (expt 2 n))
         (lista (vector->list (make-vector n 0)))
         (generar (lambda (limit lista)
                    (cond
                      ((eqv? 0 limit) empty)
                      (else
                       (cons lista (generar (- limit 1) (nextcom lista))))
                      )
                    )
                  )
         ]
      (convert-list->vector (generar limit lista))
        )
    ))
 
;;===================================================================================

;valorbool

(define valorbool
  (lambda (vec n)
    (if (< n 0)
        (not (vector-ref vec (- (abs n) 1)))
        (vector-ref vec (- n 1))
        )
    )
  )

;;===================================================================================
;reemplazar

(define reemplazar
  (lambda (vec lst)
    (cond
      [(null? lst) empty]
      [(number? lst) (valorbool vec lst)]
      [(boolean? lst) lst]
      [((list-of list?) lst)
       (cons (reemplazar vec (car lst))
             (reemplazar vec (cdr lst)))]
      [(list? lst) (cons (reemplazar vec (car lst)) (reemplazar vec (cdr lst)))]
      [else (list
              (reemplazar vec (car lst))
              (reemplazar vec (cdr lst)))]
      )
    )
  )
  
;;===================================================================================
;evaluar-or

(define evaluar-or
  (lambda (lst)
    (cond
      [(null? (cdr lst)) (car lst)]
      [else (or (car lst) (evaluar-or (cdr lst)))]
      )
    )
  )

;;===================================================================================
;evaluar-and

(define evaluar-and
  (lambda (lst)
    (cond
      [(null? (cdr lst)) (evaluar-or (car lst))]
      [else (and (evaluar-or (car lst)) (evaluar-and (cdr lst)))]
      )
    )
  )

;;===================================================================================
;eval-solve-fnc

(define eval-solve-fnc
  (lambda (lst)
    (letrec
        [
         (comb (combinaciones (car lst)))
         (evaluar (lambda (com lst)
                          (cond
                            [(null? com) (list 'insactisfactible com)]
                            [else (if (evaluar-and (reemplazar (car com) lst))
                                      (list 'satisfactible (vector->list (car com)))
                                      (evaluar (cdr com) lst)
                                      )]
                            )
                          ))
         ]
      (evaluar comb (cadr lst))
        )
    ))

;;===================================================================================
;eval-binprim

(define eval-unprim
  (lambda (op op1)
    (cases prim-un op
      (solve-fnc () (eval-solve-fnc op1))
      (add1 () (+ op1 1))
      (sub1 () (- op1 1))
      (add1_16 () (suma-bignum op1 '(1)))
      (sub1_16 () (resta-bignum op1 '(1)))
      (lenght-exp () (length op1))
      (lista?-exp () (list? op1))
      (cabeza-exp () (car op1))
      (cola-exp ()
                (letrec
                    [(recorrer
                      (lambda (lst)
                        (cond
                          [(null? (cdr lst)) (car lst)]
                          [else (recorrer (cdr lst))]
                          )
                        )
                      )]
                  (recorrer op1)
                    )
                )
      )
    )
  )

;;===================================================================================
; clousure

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env ambiente?)
   )
  )

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids (list->vector args) env)))
      )
    )
  )
  
  
;;===================================================================================
;; eval-rand-pref
(define eval-rand-pref
  (lambda (x amb)
    (cases expresion x
      (refid-exp (id) (indirect-target
                     (let
                         (
                          (ref (apply-env-ref amb id))
                          )
                       (cases target (primitive-deref ref)
                         (direct-target (expval) ref)
                         (cons-target (expval) ref)
                         (indirect-target (ref1) ref1)
                         )
                         )
                     ))
      (else
       (direct-target (eval-expresion x amb))
       )
      )
    )
  )
  
;;===================================================================================
;;eval-lista

(define eval-lista
  (lambda (l-exp env)
    (cases lista l-exp
      (empty-list () empty)
      (lista1 (lexps) (map (lambda (x) (eval-expresion x env)) lexps))
      )
    )
  )
  
;;===================================================================================
;;eval-vector

(define eval-vector
  (lambda (l-exp env)
    (cases vectorB l-exp
      (vector1 (lexps) (list->vector (map (lambda (x) (eval-expresion x env)) lexps)))
      )
    )
  )

;;===================================================================================
;;eval-registro

(define eval-registro
  (lambda (l-exp env)
    (cases registro l-exp
      (registro1 (lids lexp)
                 (if (null? lids)
                     #()
                     (letrec
                     [(armarRegistro (lambda (lids lexp)
                                       (cond
                                         [(null? lids) empty]
                                         [else (append (list
                                                        (list->vector
                                                               (list
                                                                (car lids)
                                                                (eval-expresion (car lexp) env))))
                                                       (armarRegistro (cdr lids) (cdr lexp)))]
                                         )
                                       ))]
                   (list->vector (armarRegistro lids lexp))
                     )))
      )
    )
  )

;;===================================================================================
;;eval-register?

(define eval-register?
  (lambda (exp)
    (cases expresion exp
      (registro-exp (reg)
                    (cases registro reg
                      (registro1 (lids lexp) #t)
                      (else #f)
                      )
                    )
      (else #f)
      )
    )
  )

;;===================================================================================
;;eval-vector?

(define eval-vector?
  (lambda (exp)
    (cases expresion exp
      (vector-exp (vec)
                    (cases vectorB vec
                      (vector1 (lexp) #t)
                      (else #f)
                      )
                    )
      (else #f)
      )
    )
  )

;;===================================================================================
;;eval-ref-reg

(define eval-ref-reg
  (lambda (id reg env)
    (letrec
        [(op1 id)
         (op2 (eval-registro reg env))
         (buscar-id (lambda (id ac)
                      (cond
                        [(eqv? (vector-length op2) 0) (eopl:error "El registro está vacío" id)]
                        [(eqv? ac (vector-length op2)) (eopl:error "No se encontró la clave" id)]
                        [(eqv? id (vector-ref (vector-ref op2 ac) 0))
                         (vector-ref (vector-ref op2 ac) 1)]
                        [else (buscar-id id (+ ac 1))]
                        )
                      ))]
      (buscar-id op1 0)
      )
    )
  )

;;===================================================================================
;;eval-set-reg

(define eval-set-reg
  (lambda (id reg val)
    (letrec
        [
         (buscar-id (lambda (key ac)
                      (cond
                        [(eqv? (vector-length reg) 0) (eopl:error "El registro está vacío" key)]
                        [(eqv? ac (vector-length reg)) (eopl:error "No se encontró la clave" key)]
                        [(eqv? key (vector-ref (vector-ref reg ac) 0))
                         (begin
                           (vector-set! (vector-ref reg ac) 1 val)
                           'OK!)]
                        [else (buscar-id key (+ ac 1))]
                        )
                      ))]
      (buscar-id id 0)
      )
    )
  )

;;===================================================================================
;;eval-bool-exp

(define eval-bool-exp
  (lambda (exp-bool env)
    (cases expr-bool exp-bool
                  (comparacion (pre-prim exp1 exp2)
                               (eval-pred-prim pre-prim
                                               (eval-expresion exp1 env)
                                               (eval-expresion exp2 env))
                               )
                  (conjuncion (op-bin-bool exp-bool1 exp-bool2)
                              (eval-oper-bin-bool op-bin-bool
                                                  (eval-bool-exp exp-bool1 env)
                                                  (eval-bool-exp exp-bool2 env))
                              )
                  (vlr-bool (valor)
                            (cases bool valor
                              (true-exp () #t)
                              (false-exp () #f)
                              )
                            )
                  (op-comp (op-un-bool exp-bool1)
                              (eval-oper-un-bool op-un-bool (eval-bool-exp exp-bool1 env))
                              )
                  )
    )
  )

;;===================================================================================
;;eval-pred-prim

(define eval-pred-prim
  (lambda (op op1 op2)
    (cases pred-prim op
      (menor-exp () (< op1 op2))
      (mayor-exp () (> op1 op2))
      (menor=exp () (<= op1 op2))
      (mayor=exp () (>= op1 op2))
      (igual=exp () (eqv? op1 op2))
      (diferente-exp () (not (eqv? op1 op2)))
      )
    )
  )

;;===================================================================================
;;eval-oper-bin-bool

(define eval-oper-bin-bool
  (lambda (op op1 op2)
    (cases oper-bin-bool op
      (and-exp () (and op1 op2))
      (or-exp () (or op1 op2))
      )
    )
  )

;;===================================================================================
;;eval-oper-un-bool

(define eval-oper-un-bool
  (lambda (op op1)
    (cases oper-un-bool op
      (not-exp () (not op1))
      )
    )
  )
  
;;===================================================================================
;;iteracion

(define iteracion
  (lambda (exp-bool exp env)
    (if (eval-bool-exp exp-bool env)
                        (begin
                          (eval-expresion exp env)
                          (iteracion exp-bool exp env)
                          )
                        'EndWhile
                        )
    )
  )
;;===================================================================================
;;objectos
;;===================================================================================
;;the-class-env

(define the-class-env '())

;;===================================================================================
;;elaborate-class-decls!

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))


;;===================================================================================
;;class-decl->class-name

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    class-name))))

;;===================================================================================
;;class-decl->class-name

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    super-name))))


;;class-decl->field-ids

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    field-ids))))

;;===================================================================================
;;class-decl->method-decls

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
                    m-decls))))
		    
;;===================================================================================
;eval-rand-pref

(define eval-rand-pref
  (lambda (x amb)
    (cases expresion x
      (refid-exp (id) (indirect-target
                     (let
                         (
                          (ref (apply-env-ref amb id))
                          )
                       (cases target (primitive-deref ref)
                         (direct-target (expval) ref)
                         (cons-target (expval) ref)
                         (indirect-target (ref1) ref1)
                         )
                         )
                     ))
      (else
       (direct-target (eval-expresion x amb))
       )
      )
    )
  )
;;===================================================================================
;eval-lista

(define eval-lista
  (lambda (l-exp env)
    (cases lista l-exp
      (empty-list () empty)
      (lista1 (lexps) (map (lambda (x) (eval-expresion x env)) lexps))
      )
    )
  )

;eval-vector

(define eval-vector
  (lambda (l-exp env)
    (cases vectorB l-exp
      (vector1 (lexps) (list->vector (map (lambda (x) (eval-expresion x env)) lexps)))
      )
    )
  )
;;===================================================================================
;eval-registro

(define eval-registro
  (lambda (l-exp env)
    (cases registro l-exp
      (registro1 (lids lexp)
                 (if (null? lids)
                     #()
                     (letrec
                     [(armarRegistro (lambda (lids lexp)
                                       (cond
                                         [(null? lids) empty]
                                         [else (append (list
                                                        (list->vector
                                                               (list
                                                                (car lids)
                                                                (eval-expresion (car lexp) env))))
                                                       (armarRegistro (cdr lids) (cdr lexp)))]
                                         )
                                       ))]
                   (list->vector (armarRegistro lids lexp))
                     )))
      )
    )
  )
;;===================================================================================
;eval-register?

(define eval-register?
  (lambda (exp)
    (cases expresion exp
      (registro-exp (reg)
                    (cases registro reg
                      (registro1 (lids lexp) #t)
                      (else #f)
                      )
                    )
      (else #f)
      )
    )
  )
;;===================================================================================
;eval-vector?

(define eval-vector?
  (lambda (exp)
    (cases expresion exp
      (vector-exp (vec)
                    (cases vectorB vec
                      (vector1 (lexp) #t)
                      (else #f)
                      )
                    )
      (else #f)
      )
    )
  )
;;===================================================================================
;eval-ref-reg

(define eval-ref-reg
  (lambda (id reg env)
    (letrec
        [(op1 id)
         (op2 (eval-registro reg env))
         (buscar-id (lambda (id ac)
                      (cond
                        [(eqv? (vector-length op2) 0) (eopl:error "El registro está vacío" id)]
                        [(eqv? ac (vector-length op2)) (eopl:error "No se encontró la clave" id)]
                        [(eqv? id (vector-ref (vector-ref op2 ac) 0))
                         (vector-ref (vector-ref op2 ac) 1)]
                        [else (buscar-id id (+ ac 1))]
                        )
                      ))]
      (buscar-id op1 0)
      )
    )
  )
;;===================================================================================
;eval-set-reg

(define eval-set-reg
  (lambda (id reg val)
    (letrec
        [
         (buscar-id (lambda (key ac)
                      (cond
                        [(eqv? (vector-length reg) 0) (eopl:error "El registro está vacío" key)]
                        [(eqv? ac (vector-length reg)) (eopl:error "No se encontró la clave" key)]
                        [(eqv? key (vector-ref (vector-ref reg ac) 0))
                         (begin
                           (vector-set! (vector-ref reg ac) 1 val)
                           'OK!)]
                        [else (buscar-id key (+ ac 1))]
                        )
                      ))]
      (buscar-id id 0)
      )
    )
  )
;;===================================================================================
;eval-bool-exp

(define eval-bool-exp
  (lambda (exp-bool env)
    (cases expr-bool exp-bool
                  (comparacion (pre-prim exp1 exp2)
                               (eval-pred-prim pre-prim
                                               (eval-expresion exp1 env)
                                               (eval-expresion exp2 env))
                               )
                  (conjuncion (op-bin-bool exp-bool1 exp-bool2)
                              (eval-oper-bin-bool op-bin-bool
                                                  (eval-bool-exp exp-bool1 env)
                                                  (eval-bool-exp exp-bool2 env))
                              )
                  (vlr-bool (valor)
                            (cases bool valor
                              (true-exp () #t)
                              (false-exp () #f)
                              )
                            )
                  (op-comp (op-un-bool exp-bool1)
                              (eval-oper-un-bool op-un-bool (eval-bool-exp exp-bool1 env))
                              )
                  )
    )
  )
;;===================================================================================
;eval-pred-prim

(define eval-pred-prim
  (lambda (op op1 op2)
    (cases pred-prim op
      (menor-exp () (< op1 op2))
      (mayor-exp () (> op1 op2))
      (menor=exp () (<= op1 op2))
      (mayor=exp () (>= op1 op2))
      (igual=exp () (eqv? op1 op2))
      (diferente-exp () (not (eqv? op1 op2)))
      )
    )
  )
;;===================================================================================
;eval-oper-bin-bool

(define eval-oper-bin-bool
  (lambda (op op1 op2)
    (cases oper-bin-bool op
      (and-exp () (and op1 op2))
      (or-exp () (or op1 op2))
      )
    )
  )
;;===================================================================================
;eval-oper-un-bool

(define eval-oper-un-bool
  (lambda (op op1)
    (cases oper-un-bool op
      (not-exp () (not op1))
      )
    )
  )
