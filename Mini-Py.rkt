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
