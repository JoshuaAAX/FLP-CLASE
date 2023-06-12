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
;;<prim-un>         ::= solveFNC | length
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
    (prim-un ("length") length-exp)
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
      (length-exp () (length op1))
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
;;=================================================================================
;;======================================objetos====================================
;;=================================================================================
;method-decl->method-name

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))
;;===================================================================================
;method-decl->ids

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))
;;===================================================================================
;method-decl->body

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))
;;===================================================================================
;method-decls->method-names

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
;;===================================================================================
;cambio

(define cambio
  (lambda (algo)
    (if (referencia? algo)
        (def-ref algo)
        algo
        )
    )
  )
;;===================================================================================
;lookup-class

(define lookup-class
  (lambda (name)
    (let loop
      ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
                     "Unknown class ~s" (cambio name)))
        ((eqv? (class-decl->class-name (car env)) (cambio name)) (car env))
        (else (loop (cdr env)))))))
;;===================================================================================
;define-datatype part

(define-datatype part part?
  (a-part
   (class-name symbol?)
   (fields vector?)))
;;===================================================================================
;part->class-name

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))
;;===================================================================================
;part->fields

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))
;;===================================================================================
;part->field-ids

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))
;;===================================================================================
;part->class-decl

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))
;;===================================================================================
;part->method-decls

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))
;;===================================================================================
;part->super-name

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))
;;===================================================================================
;class-name->method-decls

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))
;;===================================================================================
;class-name->super-name

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))
;;===================================================================================
;object->class-name

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))
;;===================================================================================
;cambiar-a-target

(define cambiar-a-target
  (lambda (vec)
    (list->vector (map (lambda (x) (direct-target x)) (vector->list vec)))
    )
  )
;;===================================================================================
;make-first-part

(define make-first-part
  (lambda (c-decl)
    (a-part
     (class-decl->class-name c-decl)
     (cambiar-a-target (make-vector (length (class-decl->field-ids c-decl)))))))
;;===================================================================================
;new-object

(define new-object
  (lambda (class-name)
    (if (eqv? class-name '@object)
        '()
        (let ((c-decl (lookup-class class-name)))
          (cons
           (make-first-part c-decl)
           (new-object (class-decl->super-name c-decl)))))))
;;===================================================================================
;view-object-as

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
        parts
        (view-object-as (cdr parts) class-name))))
;;===================================================================================
;build-field-env

(define build-field-env
  (lambda (parts)
    (if (null? parts)
        (empty-env)
        (extend-env
         (part->field-ids (car parts))
         (part->fields    (car parts))
         (build-field-env (cdr parts))))))
;;===================================================================================
;lookup-method-decl

(define lookup-method-decl
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
;;===================================================================================
;apply-method

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expresion body
                       (extend-env
                        (cons '%super (cons '@self ids))
                        (list->vector (cons (direct-target super-name) (cons (direct-target self) args)))
                        (build-field-env
                         (view-object-as (cambio self) (cambio host-name))))))))
;;===================================================================================
;find-method-and-apply

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name '@object)
        (eopl:error 'find-method-and-apply
                    "No method for name ~s" m-name)
        (let ((m-decl (lookup-method-decl m-name
                                          (class-name->method-decls host-name))))
          (if (method-decl? m-decl)
              (apply-method m-decl host-name self args)
              (find-method-and-apply m-name
                                     (class-name->super-name host-name)
                                     self args))))))

;;===================================================================================
;;eval-rands

(define eval-rands
  (lambda (exps env)
    (map
     (lambda (exp) (eval-expresion exp env))
     exps)
    )
  )

;;===================================================================================
;;eval-expresion

(define eval-expresion
  (lambda (pgm env)
    (cases expresion pgm
      (num-exp (n) n)
      (numerohex-exp (lnum) lnum)
      (caracter-exp (caracter) (string->symbol caracter))
      (cadena-exp (cad) cad)
      (identificador-exp (id) (apply-env env id))
      (refid-exp (id) (indirect-target
                     (let
                         (
                          (ref (apply-env-ref env id))
                          )
                       (cases target (primitive-deref ref)
                         (direct-target (expval) ref)
                         (cons-target (expval) ref)
                         (indirect-target (ref1) ref1)
                         )
                         )
                     ))
      (var-exp (ids rands body)
               (let
                   ((rands-num (map (lambda (x) (eval-rand-pref x env)) rands)))
                 (eval-expresion body (extend-env ids (list->vector rands-num) env))
                   ))
      (asignar-exp (id exp)
                   (begin
                     (set-ref!
                      (apply-env-ref env id)
                      (eval-expresion exp env))
                     'OK!))
      (cons-exp (ids rands body)
                (let (
                      (rands-num (map (lambda (x) (cons-target (eval-expresion x env))) rands))
                      )
                 (eval-expresion body (extend-env ids (list->vector rands-num) env))
                   ))
      (primbin-exp (op exp1 exp2)
                   (eval-binprim op
                                 (eval-expresion exp1 env)
                                 (eval-expresion exp2 env)))
      (proc-exp (ids body)
                (closure ids body env)
                )
      (app-exp (rator rands)
               (let
                   [
                    (proc (eval-expresion rator env))
                    (lrands (map (lambda (x) (eval-rand-pref x env))rands))
                    ]

               (if (procval? proc)
                   (apply-procedure proc lrands)
                   (eopl:error "%s no es un procedimiento" proc)
                   )
                 )
               )
      (rec-exp (proc-names idss bodies body)
               (eval-expresion body (recursively-extended-env-record proc-names idss bodies env)))
      (print-exp (exp) (begin (display (eval-expresion exp env)) (display "\n") 'endPrint))

      (fnc-exp (n claor lclaor)
               (eval-fnc n claor lclaor)
               )

      (for-exp (id exp1 tod exp2 body)
               (letrec
                   [(i (eval-expresion exp1 env))
                    (parada (eval-expresion exp2 env))
                    (op (cases to-o-downto tod
                          (to () +)
                          (downto () -)
                          ))
                    (proc-for (closure (list id) body env))
                    (for (lambda (var)
                           (if (eqv? var parada)
                               (apply-procedure proc-for (list (direct-target var)))
                               (begin (apply-procedure proc-for (list (direct-target var))) (for (op var 1)))
                               )
                               )
                           )]
                 (for i)
                   )
               )
      (begin-exp (exp lexps)
                 (if (null? lexps)
                     (eval-expresion exp env)
                     (letrec
                         [(recorrer (lambda (L)
                                      (cond
                                        [(null? (cdr L)) (eval-expresion (car L) env)]
                                        [else (begin (eval-expresion (car L) env)
                                                     (recorrer (cdr L))
                                        )]
                                        )
                                      ))
                          ]
                       (begin
                         (eval-expresion exp env)
                         (recorrer lexps))
                         )
                     )
                 )

      (lista-exp (lexps) (eval-lista lexps env))
      (vector-exp (vexps) (eval-vector vexps env))
      (registro-exp (rexp) (eval-registro rexp env))
      (ref-reg-exp (id reg) (eval-ref-reg id reg env))
      (registros?-exp (exp) (eval-register? exp))
      (isvector-exp (exp) (eval-vector? exp))
      (primun-exp (op exp) (eval-unprim op (eval-expresion exp env)))

      (if-exp (exp-bool true-exp false-exp)
              (if (eval-bool-exp exp-bool env)
                  (eval-expresion true-exp env)
                  (eval-expresion false-exp env)
                  )
              )

      (while-exp (exp-bool exp)
                 (iteracion exp-bool exp env)
                 )

      (bool-exp (exp-bool)
                (eval-bool-exp exp-bool env)
                )

      (set-vec-exp (exp1 exp2 exp3)
                   (begin
                     (vector-set! (eval-expresion exp2 env) (eval-expresion exp1 env) (eval-expresion exp3 env))
                     'OK!
                     )
                   )

      (crear-reg-exp (id exp1 rexp)
                     (list->vector
                      (append
                       (vector->list (eval-expresion rexp env))
                       (list (list->vector (list id (eval-expresion exp1 env))))
                       )
                      )
                     )

      (set-reg-exp (exp1 exp2 exp3)
                   (let
                       (
                        (id
                         (cases expresion exp1
                           (identificador-exp (s) s)
                           (else (eopl:error "no es un identificador"))
                           )
                         )
                        )
                     (eval-set-reg id (eval-expresion exp2 env) (eval-expresion exp3 env))
                     )
                   )
      (new-object-exp (class-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (new-object class-name)))
                        (find-method-and-apply
                         '@initialize class-name obj args)
                        obj))
      (method-app-exp (obj-exp method-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (eval-expresion obj-exp env)))
                        (find-method-and-apply
                         method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
                      (let ((args (eval-rands rands env))
                            (obj (apply-env-ref env '@self)))
                        (find-method-and-apply
                         method-name (apply-env-ref env '%super) obj args)
                        )
                      )
      (else (eopl:error "No es una expresión válida"))
      )
    )
  )

;;===================================================================================

;eval-program


(define eval-program
  (lambda (pgm)
    (cases BSAT pgm
      (bsat-program (lclass exp)
                    (elaborate-class-decls! lclass)
                    (eval-expresion exp init-env))
      (else (eopl:error "No es un programa BSAT valido"))
        )
    )
  )
;;===================================================================================


(define interpretador
  (sllgen:make-rep-loop
   ">>"
   (lambda (pgm) (eval-program pgm))
   (sllgen:make-stream-parser lexico gramatica)
   )
  )

(interpretador)

;;===================================================================================
;;===================================================================================

; pruebas de producciones


;(scan&parse "5");  numero-exp
;(scan&parse "x_16(4 5 3)");  numerohex-exp
;(scan&parse "'f'");  caracter-exp
;(scan&parse "\" hola \"");  cadena-exp
;(scan&parse "false");  bool-exp con false-exp
;(scan&parse "true");  bool-exp con true-exp
;(scan&parse "@x");   identificador-exp
;(scan&parse "var @x = 6 in add1(@x)");  var-exp
;(scan&parse "$@x"); refid-exp
(scan&parse "set @x = 6");  asignar-exp
(scan&parse "cons @j = 99 in print(@j)");  cons-exp
;(scan&parse "rec @f(@x)= add1(@x) in (@f 7)"); rec-exp
;(scan&parse "begin print("hola") ; print("mundo\") end");  begin-exp
;(scan&parse "begin print(\"hola\") ; print(\"mundo\") end");  begin-exp
;(scan&parse "for @x = 1 to 5 do print(@x) done");  for-exp con to
;(scan&parse "for @x = 5 downto 1 do print(@x) done");  for-exp con downto

;;===================================================================================
;(scan&parse "solveFNC(FNC 3 ((1 or 2 or 3) and (3 or 2 or 1)))");  solve-fnc
;(scan&parse "+(2,3)"); prim-exp con +
;(scan&parse "-(2,3)"); prim-exp con -
;(scan&parse "*(2,3)"); prim-exp con *
;(scan&parse "/(2,3)"); prim-exp con /
;(scan&parse "%(2,3)"); prim-exp con %
;(scan&parse "add1(2)");  prim-exp con add1
;(scan&parse "sub1(2)");  prim-exp con sub1
;(scan&parse "+_16(x_16(4 5 3), x_16(1 1))");  prim-exp con +_16
;(scan&parse "-_16(x_16(4 5 3), x_16(1 2))");  prim-exp con -_16
;(scan&parse "*_16(x_16(4 5 3), x_16(1))");  prim-exp con *_16
;(scan&parse "add1_16(x_16(4 5 3))");  prim-exp con add1_16
;(scan&parse "sub1_16(x_16(4 5 3))");  prim-exp con sub1_16
;(scan&parse "length(\"cadena\")");  prim-exp con length
;(scan&parse "concat(\"cadena\",\"cadena\")");  prim-exp con concat
;(scan&parse "empty");  prim-exp con empty
;(scan&parse "create-list(5,[])");  prim-exp con crear-lista
;(scan&parse "list?([@x,@y])");  prim-exp con lista?
;(scan&parse "head([@x,@y])");  prim-exp con cabeza
;(scan&parse "tail([@x,@y])");  prim-exp con cola
;(scan&parse "append([@x,@y],[@v,@w])");  prim-exp con append
;(scan&parse "vector?(vector[@x,@y])");  prim-exp con vector?
;(scan&parse "create-vec(5 , vector[])");  prim-exp con crear-vec
;(scan&parse "ref-vec(2,vector[@x,@y])");  prim-exp con ref-vec
;(scan&parse "register? ({@x=8})");  prim-exp con register?
;(scan&parse "create-reg(@x=8,{@d=3})");  prim-exp con crear-reg
;(scan&parse "ref-reg(@x,{@x=8})");  prim-exp con ref-reg
;;===================================================================================

;(scan&parse "set-vec(1,vector[@x,@y],5)");  prim-exp con set-vec
;(scan&parse "set-reg(@x,{@x=8},9)");  prim-exp con set-reg
;(scan&parse "proc(@x) set @x->4");  proc-exp
;(scan&parse "(@x 5)");  app-exp paso por valor
;(scan&parse "(@x $@z)");  app-exp paso por referencia
;(scan&parse "print(\"Hola\")"); print-exp
;(scan&parse "FNC 2 ((1 or 2) and (2 or 1))");  fnc-exp

;;===================================================================================
;(scan&parse "if <(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if >(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if <=(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if >=(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if ==(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if <>(2,3) then 2 else 3 end");  if-exp con pred-prim
;(scan&parse "if and(true,false) then 2 else 3 end");  if-exp con oper-bin-bool
;(scan&parse "if or(true,false) then 2 else 3 end");  if-exp con oper-bin-bool
;(scan&parse "if true then 2 else 3 end");  if-exp con true-exp
;(scan&parse "if false then 2 else 3 end");  if-exp con false-exp
;(scan&parse "if not(true) then 2 else 3 end");  if-exp con oper-un-bool

;;===================================================================================
;(scan&parse "while not(true) do 2 done");  while-exp con oper-un-bool
;(scan&parse "[4,5]");  lista-exp
;(scan&parse "vector[4,5]");  vector-exp
;(scan&parse "{@x=2;@y=5}");  registro-exp
;(scan&parse "<(2,1)");  bool-exp