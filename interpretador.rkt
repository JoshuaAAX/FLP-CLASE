#lang eopl

;******************************************************************************************
;;;;; Interpretador Taller 3

;;Joshua Chicame 
;;Damian Espinosa
;;Luisa Cardenas

;******************************************************************************************

;;Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      un-programa (exp)>
;;  <expression>    ::= <number>
;;                      <numero-lit (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= (<expression> <primitiva-binaria> <expression>)
;;                  ::= <expression>
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <expression>
;;                      < primapp-un-exp (prim-unaria exp) >
;;                  ::= Si <expression> entonces <expression> sino <expression> finSI
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;                  ::= declarar ( {<identifier> = <expression>}*(;) ) { <expression> }
;;                      <variableLocal-exp (ids rands body)>
;;                  ::= procedimiento (<identificador>*',') haga <expresion> finProc
;;                      <procedimiento-ex (ids cuerpo)>
;;                  ::= evaluar <expression> ( { <expression>}*) finEval
;;                      <app-exp (exp body)>
;;                  ::= declaraRec ( {<identifier>( {identifier}^*(,) ) = <expression>}^* ) { <expression> }
;;                      <declaraRec-exp (proc-names idss bodies letrec-body)>
;;  <primitiva-binaria>  ::= + | - | * | / | concat
;;  <primitiva-unaria>   ::= add1 | sub1 | longitud

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (text
   ("\"" (or letter whitespace "_")
              (arbno (or letter digit whitespace ":" "?" "=" "'" "_")) "\"") string)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
    (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression (text) text-lit)
    (expression (identifier) var-exp)
    (expression
     ("("  expression primitiva-binaria expression ")") primapp-bin-exp)
    (expression
     ( primitiva-unaria "(" expression ")") primapp-un-exp)
    (expression
     ("Si" expression "entonces" expression "sino" expression "finSI") condicional-exp)
    (expression
     ("declarar" "("(separated-list identifier "=" expression ";")")" "{" expression "}") variableLocal-exp)
    (expression
     ("procedimiento" "(" (separated-list identifier ",") ")" "haga" expression "finProc") procedimiento-ex)
    (expression ( "evaluar" expression "("(separated-list expression ",") ")"  "finEval" ) app-exp)
    (expression ("declaraRec" "(" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) ")" "{" expression "}") 
                declaraRec-exp)
    ;;;
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("longitud") primitiva-longitud)))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;******************************************************************************************

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)l
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

;init-env: ()--> <extend-env>
; función que retorna un ambiente inicial en forma de sintaxis abstracta.
; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @pi) 
     '(1 2 3 "hola" "FLP" 3.141592) 
     (empty-env))))

; eval-expression: <expression> <enviroment> -> numero
; evalua la expresión para la gramatica y recibe un ambiente
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (datum) datum)
      (text-lit (txt) (normalize txt))
      (var-exp (id) (search-var env id))
      (primapp-bin-exp (rand1 prim-bin rand2)
                   (let ((args  (eval-expression rand1 env))
                         (args2 (eval-expression rand2 env)))
                     (apply-primitiva-bin prim-bin args args2)))
      (primapp-un-exp (prim-un rand)
                   (let ((args (eval-expression rand env)))
                     (apply-primitiva-una prim-un args)))
      (condicional-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (variableLocal-exp (ids rands body)
              (let ((args (eval-rands rands env)))
                (eval-expression body
                                 (extend-env ids args env))))
      (procedimiento-ex (ids cuerpo)
                (cerradura ids cuerpo env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (declaraRec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env))))))

; funciones auxiliares para aplicar eval-rand a cada elemento de una lista de operandos (expresiones)
; <lista> <enviroment> -> <lista>
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

; <structure-rand> <enviroment> -> <numero>
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

; apply-primitiva-bin: <primitiva> <expression> <expression> -> numero | text
; aplica una función primitiva binaria a dos argumentos recibidos arg1 arg2
(define apply-primitiva-bin
  (lambda (prim arg1 arg2 )
    (cases primitiva-binaria prim
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-concat() (string-append arg1 arg2)) 
      )))

; apply-primitiva-una: <primitiva> <expression> -> numero
; aplica una función primitiva unaria a un argumento recibido arg
(define apply-primitiva-una
  (lambda(prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length (normalize arg)))
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
    )))

; normalize: elimina los backslash "\" de un string
(define normalize
  (lambda (s)
    (if (string-ci=? s "") ""
        (if (string-ci=? (substring s 0 1) "\"")
            (normalize(substring s 1))
            (string-append(substring s 0 1) (normalize(substring s 1)))))
    )
  )

;Define el tipo de dato Procedimiento
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (eval-expression body (extend-env ids args env))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
; <numero> -> <boolean>
(define true-value?
  (lambda (x)
    (not (zero? x))))

;******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

;Definir datos booleanos.
(define scheme-value? (lambda (v) #t))

; empty-env:    -> enviroment
; función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> extended-enviroment
;función que extiende un ambiente.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

; extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
; función que extiende un ambiente para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env))) 

;función que busca un símbolo en un ambiente
(define search-var
  (lambda (env sym)
    (cases environment env
      (empty-env-record
       ()
                        (eopl:error 'search-var "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (search-var env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (search-var old-env sym)))))))

;******************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

;list-find-position: <symbol> <list-of-symbol> -> number
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;list-index: <procedure> <list-of-symbol> -> number | boolean
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
                
 (show-the-datatypes)
just-scan
scan&parse
                
                
;******************************************************************************************
;EVALUABLE

; PUNTO A)
; calcular el area de un circulo
; int -> int
;
;declarar(
;  @radio=2.5;
;  @areaCirculo=
;       procedimiento(@radio)
;            haga (@pi * (@radio * @radio))
;       finProc
;){
;
;  evaluar @areaCirculo(@radio) finEval
;}
;
;ejemplos
;@radio=2 = 12.566368;
;@radio=4 = 50.265472;


;******************************************************************************************

; PUNTO B
; Calcular el factorial mediante recursión
; se multiplica desde el número  hasta el parametro de entrada
;declaraRec(
;           @factorial(@n) = Si @n entonces (@n*evaluar @factorial(sub1(@n)) finEval) sino 1 finSI
;           ){
;             evaluar @factorial(5) finEval
;             }
;ejemplos
;evaluar @factorial(5) finEval = 120
;evaluar @factorial (10) finEval = 3628800

;******************************************************************************************

; PUNTO C
; procedimiento que permite calcular una suma de forma recursiva
;
;declaraRec(
;           @sumar(@a, @b) = Si @a entonces evaluar @sumar(sub1(@a), add1(@b)) finEval sino @b finSI
;           ){
;             evaluar @sumar(4, 5) finEval
;             }
;
;ejemplos
;evaluar @sumar(4, 5) finEval = 9
;evaluar @sumar(6, 8) finEval = 14

; Punto D

; Punto E
; funcion que muestra los nombres de los integrantes del grupo.
;
;declarar(
;     @integrantes = procedimiento() haga "Joshua Damian Luisa" finProc
;){ 
;   declarar(
;     @saludar = procedimiento(@integrantes) haga
;        procedimiento() haga ("Hola " concat evaluar @integrantes() finEval) finProc
;        finProc
;   ){
;      declarar(
;        @decorate = evaluar @saludar(@integrantes) finEval 
;      ){
;        evaluar @decorate() finEval
;      } 
;   }
;}

; Punto F

(interpretador)