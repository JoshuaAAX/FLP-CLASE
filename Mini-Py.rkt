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
