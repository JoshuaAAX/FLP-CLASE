#lang eopl

;******************************************************************************************
;;;;; Interpretador Taller 3

;; repositorio: https://github.com/JoshuaAAX/FLP-CLASE/

;;Joshua Chicame 2074121
;;Damian Espinosa 2028180
;;Luisa Cardenas 1823494

;;*********************************Gramatitca**************************************

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