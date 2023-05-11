#lang eopl

;******************************************************************************************
;;;;; Interpretador Taller 3

;;Joshua Chicame 2074122
;;Damian Alessandro
;******************************************************************************************

;<programa> :=  <expresion>
;               un-programa (exp)

;<expresion> := <numero>
;               numero-lit (num)
;            := "\""<texto> "\""
;               texto-lit (txt)
;            := <identificador>
;               var-exp (id)
;            := (<expresion> <primitiva-binaria> <expresion>)
;               primapp-bin-exp (exp1 prim-binaria exp2)
;            := <primitiva-unaria> (<expresion>)
;               primapp-un-exp (prim-unaria exp)


;<primitiva-binaria> :=  + (primitiva-suma)
;                    :=  ~ (primitiva-resta)
;                    :=  / (primitiva-div)
;                    :=  * (primitiva-multi)
;                    :=  concat (primitiva-concat)


;<primitiva-unaria>:=  longitud (primitiva-longitud)
;                  :=  add1 (primitiva-add1)
;                  :=  sub1 (primitiva-sub1)


;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp (whitespace) skip)
  (comment  ("%" (arbno (not #\newline))) skip)
  (identifier ("@" (arbno(or letter digit))) symbol)
  (text  (letter (arbno (or letter digit "?"))) string)
  (number  (digit (arbno (or digit "." )) )number)
  (number ("-" digit (arbno (or digit "." )) )number)
  ))


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    ;;Programa
    (program (expression) a-program)
 
    ;;Expresion
    (expression (number) lit-exp)
    (expression ("\"" text "\"") lit-text)
    (expression (identifier) var-exp)
    (expression ( "(" expression primitiva-binaria expression ")") primapp-bin-exp)
    (expression (primitiva-unaria "(" expression ")") primapp-un-exp)

    ;;Primitiva
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/" ) primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1" ) primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    )
  )


;Construir datatypes automaticamente

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))
