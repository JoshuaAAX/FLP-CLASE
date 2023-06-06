#lang eopl
;;INTERPRETADOR
;;Joshua Chicame 2074121
;;Luisa Cardenas
;;Damian Espinosa 2028180

;******************************************************************************************

;; La definición BNF para las expresiones del lenguaje:

;;  <programa>     ::= {<class-decl>}* <expresion>
;;                     <un-programa (class-decl exp)>

;; <class-decl>    ::= class <identificador> extends <identificador> {field <identificador>}* {<method-decl>}*
;;                     <a-class-decl(class-name super-name fields-ids method-decls)>

;; <method-decl>   ::= method <identificador> ( {<identificador>}*(,) ) <expresion>
;;                     <a-method-decl (method-name ids body)>

;; <expresion>     ::= <numero>
;;                     <numero-lit  (num)>

;;                 ::= crea-bignum( <numero> )
;;                     <bignum-exp  (num)>

;;                 := "\""<texto> "\""
;;                     <texto-lit (txt)>

;;                 ::= <identificador>
;;                     <id-exp (id)>

;;                 ::= "false"
;;                     <false-exp>

;;                 ::= "true"
;;                     <true-exp>

;;                 ::= <primitiva>(<expression>*(,))
;;                     <primapp-exp (expPrim)>

;;                 ::= if <expresion-bool> then {<expresion>} else {<expression>} end
;;                     <condicional-exp (test-exp true-exp false-exp)>

;;                 :=  procedimiento (<identificador>*',') haga <expresion> finProc
;;                     <procedimiento-ex (ids cuerpo)>

;;                 :=  evaluar <expresion>(<expresion> ",")* finEval
;;                     <app-exp(exp exps)>

;;                 ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp proc-names idss bodies bodyletrec>

;;                 ::= var {<identificador> = <expresion> }*(;) in <expresion>
;;                     <var-exp idsVar expsVar cuerpoVar>

;;                 ::= const {<identificador> = <expresion> }*(;) in <expresion>
;;                     <const-exp idsConst expsConst cuerpoConst>

;;                 ::= [{<expresiones>} *(;)]
;;                     <lista expsLista>

;;                 ::= tupla[{<expresion>}*(;)]
;;                     <tupla expsTupla>

;;                 ::= {{<identificador> = <expresion>} +(;) }
;;                     <registro idsReg expReg>

;;                 ::= begin {<expresion>}+(;) end
;;                     <secuencia expSec>

;;                 ::= while <expresion-bool> do { <expresion>}done
;;                     <while-exp expBoolWhile expWhile>

;;                 ::= for <identificador> = <expresion>  to <expresion> do {<expresion>} done
;;                     <for-exp idFor inicioFor finFor cuerpoFor>

;;                 ::= set <identificador> = <expresion>
;;                     <set-exp idSet expSet>

;;                 ::= new <identificador> ({<expresion}*(,))
;;                     <new-object-exp (class-name rands)>

;;                 ::= send <expresion> <identificador> ({<expresion>}*(,))
;;                     <method-app-exp (obj-exp method-name rands)>

;;                 ::= super <identificador> ( {<expresion>}*(,))
;;                    <super-call-exp (method-name rands)>

;; <primitiva>     ::= + (primitiva-suma)
;;                 ::= ~ (primitiva-resta)
;;                 ::= / (primitiva-div)
;;                 ::= * (primitiva-multi)
;;                 ::= % (primitiva-mod)
;;                 ::= concat(primitiva-concat)
;;                 ::= longitud(primitiva-longitud)
;;                 ::= add1(primitiva-add1)
;;                 ::= sub1(primitiva-sub1)
;;                 ::= null (primitiva-null)
;;                 ::= null? (primitiva-null?)
;;                 ::= head (primitiva-head)
;;                 ::= head-list primitiva-head-list)
;;                 ::= tail (primitiva-tail)
;;                 ::= tail-list (primitiva-tail-list)
;;                 ::= append (primitiva-append)
;;                 ::= lista? (primitiva-lista?)
;;                 ::= tupla? (primitiva-tupla?)
;;                 ::= registro? (primitiva-registro?)
;;                 ::= len (primitiva-len)

;; <pred-prim>     ::= < (pred-prim-menor)
;;                 ::= > (pred-prim-mayor)
;;                 ::= <= (pred-prim-menor-igual)
;;                 ::= >= (pred-prim-mayor-igual)
;;                 ::= == (pred-prim-igual)
;;                 ::= != (pred-prim-dif)

;;<oper-bin-bool>  ::= and (and-oper-bool)
;;                 ::= or (or-oper-bool)

;;<oper-un-bool>   ::= not (not-oper-bool) 


;;<expresion-bool> ::= <pred-prim> ( <expresion> , <expresion> )
;;                       <predicado-no-condicional expre1 expre2>

;;                 ::= <oper-bin-bool> ( <expresion-bool> , <expresion-bool> )
;;                      <predicado-bin-condicional expre1 expre2>

;;                 ::= <oper-un-bool> (<expresion-bool> )
;;                      <predicado-un-condicional expre>

;;<crea-bignum>    ::= x8 (octa-exp)

;;                 ::= x16 (hexa-exp)

;;                 ::= x32 (triges-exp) 

;;<primbin-bignum> := sum-bignum (sum-bignum)

;;                 := sub-bignum (sub-bignum)

;;                 := mult-bignum (mult-bignum)

;;                 := pot-bignum (pot-bignum)

;;<primun-bignum> := succes (succes)

;;                := predes (predes)

;*******************************************************************************************


;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  (white-sp    (whitespace) skip)
  (comentario     ("#" (arbno (not #\newline))) skip)
  (identificador  ("@" letter (arbno (or letter digit))) symbol)
  (texto        (letter (arbno (or letter digit ":" "?" "=" "'" "#" "$" "&" "." "," ";" "*" "!" "¡" "¿" "-" "_"))) string)
  (numero       (digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit)) number)
  (numero       (digit (arbno digit) "." digit (arbno digit)) number)
  (numero       ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    
    (programa ((arbno class-decl) expresion) un-programa)

    ;;class-decl
    
    (class-decl ("class" identificador "extends" identificador (arbno "field" identificador ) (arbno method-decl)) a-class-decl)

    ;;method-decl

    (method-decl ("method" identificador "("  (separated-list identificador  ",") ")" expresion )  a-method-decl)

    ;;Expresiones para Objetos

    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-object-exp)

    (expresion ("send" expresion identificador "("  (separated-list expresion ",") ")") method-app-exp)

    (expresion ("super" identificador   "("  (separated-list expresion ",") ")")  super-call-exp)
    

    ;;Expresion
    
    (expresion (numero)   numero-lit)

    (expresion (crea-bignum "(" (arbno numero) ")") bignum-exp)

    ;Para el manejo primitivas bignum
    (expresion (primbin-bignum "(" expresion "," "(" (arbno numero) ")" ")") controlbin-bignum)
    
    (expresion (primun-bignum "(" expresion ")" ) controlun-bignum)
    
    (expresion (identificador)   id-exp)

    (expresion ("\""texto"\"")   texto-lit)

    (expresion ("false") false-exp)
    
    (expresion ("true") true-exp)

    (expresion (primitiva "(" (separated-list expresion ",") ")")  primapp-exp)

    (expresion ("if" expresion-bool "then""{" expresion "}" "else" "{" expresion "}" "end") condicional-exp)

    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-ex)
    
    (expresion ("evaluar"  expresion "("(separated-list expresion ",") ")" "finEval") app-exp)

    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion )  "in" expresion) letrec-exp)

    (expresion ("var" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) var-exp)
    
    (expresion ("const" "{" (arbno identificador "=" expresion ";") "}" "in" expresion) const-exp)

    (expresion ("[" (separated-list expresion ",") "]") lista)

    (expresion ("tupla" "[" (separated-list expresion ",") "]") tupla)

    (expresion ("{" "{"identificador "=" expresion "}"";" (arbno "{"identificador "=" expresion "}"";") "}") registro)

    (expresion ("begin" "{" expresion ";" (arbno expresion ";") "}" "end") secuencia-exp)

    (expresion ("set" identificador "=" expresion) set-exp)

    (expresion ("while" expresion-bool "do" "{" expresion "}" "done" ) while-exp)

    (expresion ("for" identificador "=" expresion "to" expresion "do" "{" expresion "}""done") for-exp)


    ;;Expresion bool

    (expresion-bool (pred-prim "("expresion "," expresion")") predicado-no-condicional)
    (expresion-bool (oper-bin-bool "(" expresion-bool "," expresion-bool ")") predicado-bin-condicional)
    (expresion-bool (oper-un-bool "(" expresion-bool ")") predicado-un-condicional )

    ;;pred-prim
    (pred-prim ("<") pred-prim-menor)
    (pred-prim (">") pred-prim-mayor)
    (pred-prim ("<=") pred-prim-menor-igual)
    (pred-prim (">=") pred-prim-mayor-igual)
    (pred-prim ("==") pred-prim-igual)
    (pred-prim ("!=") pred-prim-dif)

    ;;oper-bin-bool
    (oper-bin-bool ("and") and-oper-bool)
    (oper-bin-bool ("or") or-oper-bool)

    ;;oper-un-bool
    (oper-un-bool ("not") not-oper-bool) 

    ;Primitivas bignum
    (crea-bignum ("x8") octa-exp)
    (crea-bignum ("x16") hexa-exp)
    (crea-bignum ("x32") triges-exp)
    (primbin-bignum ("sum-bignum") sum-bignum)
    (primbin-bignum ("sub-bignum") sub-bignum)
    (primbin-bignum ("mult-bignum") mult-bignum)
    (primbin-bignum ("pot-bignum") pot-bignum)
    (primun-bignum ("succes") succes)
    (primun-bignum ("predes") predes)
    
    ;;Primitiva
    
        ;;Primitiva

    (primitiva ("print-obj") primitiva-print-obj)

    (primitiva ("print")   primitiva-print)

    ;;Primitiva numeros

    (primitiva ("+")      primitiva-suma)
    (primitiva ("~")      primitiva-resta)
    (primitiva ("/")      primitiva-div)
    (primitiva ("*")      primitiva-multi)
    (primitiva ("%")      primitiva-mod)
    (primitiva ("add1")   primitiva-add1)
    (primitiva ("sub1")   primitiva-sub1)

    ;;Primitiva cadenas
    
    (primitiva ("concat") primitiva-concat)
    (primitiva ("longitud")  primitiva-longitud)

    ;;Primitiva Listas y tuplas
    
    (primitiva ("null") primitiva-null)
    (primitiva ("null?") primitiva-null?)

    ;;primitiva lista
    (primitiva ("lista?") primitiva-lista?)
    (primitiva ("cons") primitiva-crear-lista)
    (primitiva ("append") primitiva-append)
    (primitiva ("ref-list") primitiva-ref-list)
    (primitiva ("set-list") primitiva-set-list)
    (primitiva ("head-list") primitiva-head-list)
    (primitiva ("tail-list") primitiva-tail-list)
    (primitiva ("len") primitiva-len)
    ;falta tail

    ;;primiiva tupla
    (primitiva ("tupla?") primitiva-tupla?)
    (primitiva ("crear-tupla") primitiva-crear-tupla)
    (primitiva ("ref-tupla") primitiva-ref-tupla)
    (primitiva ("head") primitiva-head)
    (primitiva ("tail") primitiva-tail)

    ;;primitiva registro
    (primitiva ("registro?") primitiva-registro?)
    (primitiva ("registro") primitiva-crear-registro)
    (primitiva ("ref-registro") primitiva-ref-registro)
    (primitiva ("set-registro") primitiva-set-registro)
    
   
  )
)
    
;******************************************************************************************

;El Interprete

;eval-programa: <programa> -> expresion
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (c-decls exp)
                 (set! lista-constantes '())
                 (elaborate-class-decls! c-decls)
                 (eval-expresion exp (init-env))
      )
    )
  )
)



; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
      '(@a @b @c @d @e)
      (list 1 2 3 "Hola" "FLP")
      (empty-env)
    )
  )
)


;eval-expresion: <expresion> <enviroment> ->  
; evalua la expresión en el ambiente de entrada, para cada caso (numero-lit,var-exp,texto-lit, condicional-exp, variableLocal-exp
;procedimiento-ex, app-exp, letrec, primapp-bin-exp, primapp-un-exp) devuelve algo diferente dependiendo del caso de la expresión.

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
     
      (numero-lit (numero) numero)

      (bignum-exp (exponente numeros) numeros)

      (controlbin-bignum (operador rands1 rands2) (apply-prim-bin-bignum operador (get-Bignum-estruct rands1) rands1 rands2 env))
      
      (controlun-bignum (operador bignums) (apply-prim-una-bignum operador (get-Bignum-estruct bignums) (eval-expresion bignums env)))
  
      (id-exp (id)(apply-env env id)  )
      
      (texto-lit (txt) txt)

      (true-exp () #t)
      
      (false-exp () #f)

      (primapp-exp (prim exp)                   

                   (cases primitiva prim

                       ;Para Registros
                       (primitiva-ref-registro () (let
                                                      (
                                                       (ids (vector->list (car (eval-expresion (car exp) env))) )
                                                       (vals (vector->list (cadr (eval-expresion (car exp) env))) )
                                                       )
                                                    (eval-expresion (cadr exp) (extend-env ids vals env) )
                                                    
                        )
                      )

                     (primitiva-set-registro () (let
                                                      (
                                                       (ids (vector->list (car (eval-expresion (car exp) env))) )
                                                       (vals (vector->list (cadr (eval-expresion (car exp) env))) )
                                                       (dic (eval-expresion (car exp)   env ))
                                                       (id (cases expresion (cadr exp) (id-exp (id) id) (else #f) ))
                                                       (val (eval-expresion (caddr exp) env))
                                                       )
                                                    (begin
                                                            (let ((pos (rib-find-position id ids)))
                                                                 (if (number? pos)
                                                                  (vector-set! (cadr dic) pos val ) 
                                                                 "error"))
                                                            1)                       
                        ))

                     ;El resto de primitivas
                     (else
                       (let ((args (eval-primapp-exp-rands exp env)))
                       (apply-primitiva prim args env)
                     )
                     )
                   )
                   
                   
                  
      )

      (lista (exp) (let ((args (eval-primapp-exp-rands exp env)))
                     (if (not (null? args))
                     (apply-lista (list->vector args) )
                     #()
                     )))

      (tupla (exp) (let ((args (eval-primapp-exp-rands exp env)))
                     (if (not (null? args)) 
                     (list (car args) (cadr args) )
                     '())))

      (registro (id exp list-id list-exp)
                (let (
                      (args (eval-primapp-exp-rands list-exp env))
                      (arg (eval-expresion exp env))
                      )
                     (apply-registro id arg list-id args ))

      )
      
      (condicional-exp (exp-bool true-exp false-exp)
                       (if (eval-expresion-bool exp-bool env)
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)
                       ))

      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env))) 

      (app-exp (exp exps)
               (let ((proc (eval-expresion exp env))
                     (args (eval-rands exps env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc)
                  )
               )
       )

      (procedimiento-ex (ids cuerpo) (cerradura ids cuerpo env))

      (var-exp (ids exps cuerpo)
               (let ((args (eval-let-exp-rands exps env)))
                    (eval-expresion cuerpo (extend-env ids args env))
               )
       )

      (const-exp (ids rands body)
                 (begin
                   (set! lista-constantes (append lista-constantes ids))
                   (let ((args (eval-let-exp-rands rands env)))
                     (eval-expresion body (extend-env ids args env)))
                   )
               )

      (set-exp (id rhs-exp)
               (begin
                 (cond
                   [(buscar-elemento lista-constantes id) (eopl:error 'eval-expresion
                                 "No es posible modificar una constante" )]
                   [else (setref!
                  (apply-env-ref env id)
                  (eval-expresion rhs-exp env))])
                 1
                 ))
      
      
      (secuencia-exp (exp exps) 
                 (let loop ((acc (eval-expresion exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expresion (car exps) 
                                               env)
                              (cdr exps)))))

      (while-exp (exp-bool exp)
                  (let   loop ((i 0))
                 
                   (when (eval-expresion-bool exp-bool env)
                      (eval-expresion exp env)
                      (loop (+ 1 i))
                    )
               )
       )
      

      (for-exp ( exp desde hasta cuerpo)
         (let
             ((de (eval-expresion desde env))
                   (to (eval-expresion hasta env)))

            (let   loop ((i de))
                 
                   (when (< i to)
                      (eval-expresion cuerpo (extend-env (list exp) (list i) env))
                      (loop (+ 1 i))
                    )
               )


         )         
      )
;******************************************************************************************

;******************************************************************************************
