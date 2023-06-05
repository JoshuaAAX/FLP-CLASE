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

;******************************************************************************************

;******************************************************************************************

;******************************************************************************************
