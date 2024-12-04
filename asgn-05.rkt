#lang typed/racket
(require typed/rackunit)

#|
Program: Assignment-04-AAQZ
Progress: Completed. 
|#

;###################################################################################################
;                                       Data Definitions
;###################################################################################################
#|
EXPR = num                ;Real
 | id                     ;Symbol
 | string                 ;String
 | {if EXPR EXPR EXPR}    
 | {bind clause* EXPR}    ;local ids/vars, fun def & call, desugars into {fundef arg...},EXPR=body 
 | {(id*) => EXPR}        ;Fun def, id* = params EXPR=body
 | {EXPR EXPR*}           ;Fun call
clause ::= [id = EXPR]    ;Map param to arg 
id != if, =>, bind, =
'*' means 0 or more repetitions
|#
;-----------------------------Exprs-------------------------------------------;
(define-type ExprC (U NumC IdC StrC AppC IfC LamC))
(struct NumC ([n : Real])#:transparent)
(struct IdC  ([s : Symbol])#:transparent)
(struct StrC ([s : String])#:transparent)
(struct AppC ([fun : ExprC][args : (Listof ExprC)])#:transparent)
(struct IfC  ([if : ExprC][then : ExprC][else : ExprC])#:transparent)
(struct LamC ([params : (Listof Symbol)][body : ExprC])#:transparent)
;-----------------------------Values-------------------------------------------;
(define-type Value (U NumV BoolV StrV CloV PrimV))
(struct NumV ([n : Real])#:transparent)
(struct BoolV([b : Boolean])#:transparent)
(struct StrV ([s : String])#:transparent)
(struct CloV ([params : (Listof Symbol)][body : ExprC][env : Environment])#:transparent)
(struct PrimV([op : Symbol])#:transparent)
;-----------------------------Binding and Environment--------------------------;
(struct Bind ([name : Symbol] [val : Value])#:transparent)
(define-type-alias Environment (Listof Bind))
(define extend-env cons)                   ;"Extendable environment" ment for addint to envs
(define top-env (list (Bind '+ (PrimV '+)) ;Mount environment with bindings of all the primitives
                      (Bind '- (PrimV '-))
                      (Bind '* (PrimV '*))
                      (Bind '/ (PrimV '/))
                      (Bind '<= (PrimV '<=))
                      (Bind 'equal? (PrimV 'equal?))
                      (Bind 'true  (BoolV #t))
                      (Bind 'false (BoolV #f))
                      (Bind 'error (PrimV 'error))
                      (Bind 'println (PrimV 'println))
                      (Bind 'read-num (PrimV 'read-num))
                      (Bind 'read-str (PrimV 'read-str))
                      (Bind 'seq (PrimV 'seq))
                      (Bind '++ (PrimV '++))))

;###################################################################################################
;                                      top-interp
;###################################################################################################
; Combines parsing and evaluation of the program and returns a string rep. of a value.
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;###################################################################################################
;                                           Parse
;###################################################################################################
; Given an s-expression parses the value and returns the corresponding expression.
(define (parse [s : Sexp]): ExprC
  (match s
    [(? real?) (NumC s)] 
    [(? string?) (StrC s)]
    [(? symbol?)
     ;Check for valid id, valid-id errors on #f. 
     (IdC (valid-id s))]
    [(list 'if e1 e2 e3) (IfC (parse e1) (parse e2) (parse e3))]
    ;Adding locals - desugar into AppC
    [(list 'bind clauses ... exp)
     (define c-lst (cast clauses (Listof Sexp)))
     ;Extract info from clauses
     (define param-lst (extract-syms c-lst))
     (define arg-lst (extract-vals c-lst))
     (valid-nodups param-lst)
     ;Parse the clauses
     (define p-args (for/list : (Listof ExprC) ([arg : Sexp (in-list arg-lst)])
                      (parse arg)))
     (AppC (LamC param-lst (parse exp)) p-args)]
    ;Add lambdas
    [(list (list (? symbol? ids) ...) '=> exp)
     (define param-lst (cast ids (Listof Symbol)))
     ;Check ids
     (for-each (lambda (param) (valid-id param))
               param-lst)
     (valid-nodups param-lst)
     (LamC param-lst (parse exp))]
    ;AppC
    [(cons f r)
     (define parsed-args (for/list : (Listof ExprC) ([arg : Sexp (in-list r)]) 
                           (parse arg)))
     (AppC (parse f) parsed-args)]))

;###################################################################################################
;                                       interp
;###################################################################################################
; Given an expression and an Environment
; returns a Value using the bindings in the environment to resolve applications.
(define (interp [exp : ExprC][env : Environment]): Value
  (match exp
    [(NumC n) (NumV n)]
    [(IdC s)  (lookup s env)]
    [(StrC s) (StrV s)]
    [(AppC fun args)
     ;Get the bind value from the fun expr
     (define funVal (interp fun env))
     ;Get the list of arg values from the env.
     (define argVals (for/list : (Listof Value) ([arg : ExprC (in-list args)])
                       (interp arg env)))
     ;Determine what Value funVal is, only PrimV and CloV can be AppC's
     (match funVal 
       [(PrimV s)
        (match s
          [(or '+ '- '/ '* '<=) (p-arith s argVals)]
          ['equal? (p-equal argVals)]
          ['error (p-error funVal)]
          ['println (p-println argVals)]
          ['read-num (p-read-num)]
          ['read-str (p-read-str)]
          ['seq (p-seq argVals)]
          ['++ (p-++ argVals)])]
       [(CloV params body ce)
        (valid-arity (length params) argVals)
        ;Combine the list of symbols and values
        (define temp-env (for/list : Environment
                           ([sym : Symbol (in-list params)][val : Value (in-list argVals)])
                           (Bind sym val)))
        ;interp the function as a closure
        (interp body (bld-env temp-env ce))]
       [_ (err 'interp 'Invalid_AppC "AppC is not a primv or clov, Given: ~e" exp)])]
    [(LamC params body) (CloV params body env)]
    [(IfC e1 e2 e3)
     ;get the value of the conditional test
     (define result (interp e1 env))
     ;Apply the result to the if statement, if result is not a bool err
     (cond [(BoolV? result)
            (if (BoolV-b result) (interp e2 env) (interp e3 env))]
           [else (err 'interp
                      'Invalid_Cond
                      "Conditional: ~e, Exp: boolean result, Actual:~v" e1 result)])]))

;###################################################################################################
;                                           Helper Functions
;###################################################################################################
;Given a Value, returns the String representation of the value
(define (serialize [v : Value]): String
   (match v
     [(NumV n) (~v n)]
     [(StrV s) (~v s)]
     [(BoolV b) (match b
                  [#t "true"]
                  [#f "false"])]
     [(PrimV _) "#<primop>"]
     [(CloV _ _ _) "#<procedure>"]))

;Given a symbol returns true if it can be an id, false otherwise
(define (id? [s : Symbol]): Boolean
  (define inv-id (list 'if '=> 'bind '=))
  (match (memq s inv-id) [#f #t][_ #f]))

;Given a symbol uses the id? and errors if not an id.
(define (valid-id [s : Symbol]): Symbol
  (cond
    [(id? s) s]
    [else (err 'parse 'Invalid_ID "id != if, =>, bind, =. Given ~e" s)]))

;Given a list, checks for duplicates. errors if dups are found.
(define (valid-nodups [params : (Listof Any)]): Boolean
  (define result (check-duplicates params))
  (cond
   [(equal? result #f) #t]
   [else (err 'valid-nodups 'Dups_Found "The list contains multiple ~e's" result)]))

;err: Symbol Symbol String Any * -> Nothing
;Prints a custom error message for AAQZ and errors with make-exn:fail
(define (err [who-sym : Symbol] [type : Symbol] [format-str : String] . [v : Any *]): Nothing
  (define msg (apply format format-str v))
  (error who-sym (string-append "AAQZ[~s]: " msg) type))

;Check the environment for a symbol
(define (lookup [s : Symbol][env : Environment]): Value
  (match env
    ;Base: Cant find symbol in any of the binding
    ['() (err 'look-up
              'Undefined_Sym
              "Could not find ~e in the environment ~e" s env)]
    ;Case: A binding exists, check if symbol matches the binding 
    [(cons f r) (define name (Bind-name f))
                (define value (Bind-val f))
                (cond
                  [(equal? s name) value]
                  [else (lookup s r)])]))

;Given an two environments temp and env, extends the env to include the temp env. 
(define (bld-env [temp-env : Environment][env : Environment]): Environment
  (match temp-env
    ;Base: The temp-env is empty, nothing to add to the env enviornment.
    ['() env]
    ;Case: There is a binding to add to the enviornment, then check again. 
    [(cons f r) (cast (extend-env f (bld-env r env)) Environment)]))

;Given a list of s-expressions returns a list of all the closure ids.
(define (extract-syms [clauses : (Listof Sexp)]) : (Listof Symbol)
  (match clauses
    ;Base: No more clauses in list
    ['() '()]
    ;Case: There is a clause, save the symbol to the list.
    [(list (list (? symbol? sym) '= val) rest ...) (cons (valid-id sym) (extract-syms rest))]))

;Given a list of s-expressions returns a list of all the closure values.
(define (extract-vals [clauses : (Listof Sexp)]) : (Listof Sexp)
  (match clauses
    ;Base: No more clasues in list
    ['() '()]
    ;Case: There is a clause, save the value to the list.
    [(list (list (? symbol? sym) '= val) rest ...) (cons val (extract-vals rest))]))

;###################################################################################################
;                                       Top Env Primitive Functions
;###################################################################################################
;Given a symbol and a list of values returns the value of applying the symbol.
;Each function has an arity of two reals or else errors.
(define (p-arith [s : Symbol][args : (Listof Value)]): Value
  ;Make checks 
  (valid-arity 2 args)
  ;Get the Real from the NumV (Valid methods error on failure)
  (define op1 (NumV-n (cast (valid-value NumV? (first args)) NumV)))
  (define op2 (NumV-n (cast (valid-value NumV? (second args)) NumV)))
  ;Apply (interp) it to operands to get a Value
  (match s
    ['+  (NumV (+ op1 op2))]
    ['-  (NumV (- op1 op2))]
    ['*  (NumV (* op1 op2))]
    ['/  (NumV (/ op1 (valid-div op2)))]
    ['<= (BoolV(<= op1 op2))]))

;Given a list of Values returns a BoolV of true if both operators are equal.
(define (p-equal [args : (Listof Value)]) : BoolV
  ;Check arity
  (valid-arity 2 args)
  ;Get the Value
  (define op1 (first args))
  (define op2 (second args))
  ;Check if values are closures, primitives, and if are equal.
  (cond
    [(or (CloV? op1)(CloV? op2)) (BoolV #f)]
    [(or (PrimV? op1)(PrimV? op2)) (BoolV #f)]
    [else
     (BoolV (equal? op1 op2))]))

;Given a value, calls error with the serialized value as the message. Nothing is returned.
(define (p-error [v : Value]): Nothing
  (define ser-v (serialize v)) 
  (error 'user-error "~e" ser-v))

;Given a string, prints the string with a new line. 
(define (p-println [args : (Listof Value)]): StrV
  ;Check the arity
  (valid-arity 1 args)
  ;Check for a string
  (define str-maybe (first args))
  (match str-maybe
    [(StrV s) (display s)
              (newline)
              str-maybe]
    [_ (err 'p-print-ln
               'Invalid_Arg
               "{println <String> Wrong argument given to println ~e}" str-maybe)]))

;Prints the prompt ">", then reads a line of numeric info from the terminal
;returns the number read or signals an error if not a number
(define (p-read-num): NumV
  (printf ">")
  (define result (read-line))
  ;Check for String or EOF
  (cond
    [(string? result)
     ;Check if the string is a number
     (define num-maybe (string->number result))
     (cond
       [(real? num-maybe) (NumV num-maybe)]
       [else (err 'read-num
                  'Invalid_NumString
                  "The string read did not contain numbers ~e" result)])]
    ;EOF Occured
    [else (NumV 0)]))

;Prints the prompt ">", then reads a line of input from the termial
;returns the string without the new line.
(define (p-read-str): StrV
  (printf ">")
  (define result (read-line (current-input-port) 'linefeed))
  (cond
    [(string? result) (StrV result)]
    ;EOF occured, should we error?
    [else (StrV "")]))

;Given a any number of expressions,returns the value of the last expression. 
(define (p-seq [args : (Listof Value)]): Value
  (valid-min-arity 1 args)
  (last args))

;Joins together the arguments into a string, numbers are converted to strings
(define (p-++ [argVals : (Listof Value)]): StrV
  ;Check for at least one arg
  (valid-min-arity 1 argVals)
  ;Turn values into strings
  (define argStr (for/list : (Listof String)
                           ([val : Value (in-list argVals)])
                           (match val
                             [(NumV n)  (number->string n)]
                             [(StrV s) s]
                             [(BoolV b) (format "~a" b)]
                             [(CloV p b e) (format "{(~a)=>(~a)}" p b)]
                             [(PrimV s) (format "~a" s)])))
  ;Return the new string
  (StrV (string-append* argStr)))

;###################################################################################################
;                                       Top Env Primitive Function Helpers
;###################################################################################################
;Given a required arity of params, and a list of argument Values
;returns the arity or errors if arities dont match. 
(define (valid-arity [arity : Real][args : (Listof Value)]): Real
  (define len (length args))
  (cond
    [(equal? arity len) arity]
    [else (err 'valid-arity
               'Invalid_Arity
               "Exp: ~e, Act: ~e, ~e" arity len args)]))

;Given a required min arity of params, and a list of argument Values
;returns the arity or errors if arities does not meet the min.
(define (valid-min-arity [arity : Real][args : (Listof Value)]): Real
  (define len (length args))
  (cond
    [(>= len arity) len]
    [else (err 'valid-min-arity
               'Invalid_Arity
               "Minimum arity needed is ~e, given arity of ~e in args ~e" arity len args)]))

;Given a predicate function and a Value
;returns the Value or errors if the Value does not pass the predicate. 
(define (valid-value [pred : (Any -> Boolean)] [value : Value]): Value 
  (cond
    [(pred value) value]
    [else (err 'valid-numv
               'Invalid_Value
               "Exp: (~e <real>), Act: ~e" pred value)]))

;Given a Real divisor, returns the devisor or erros if division by zero. 
(define (valid-div [divisor : Real]): Real
  (match divisor
    [0 (err 'valid-div
               'Div_By_Zero
               "Cannot divide by zero!")]
    [else divisor]))

;###################################################################################################
;                                       Test Program
;###################################################################################################
(define example-program
  '{seq {println "Lets play a game! \nRock-Paper-Scissors"}
        {println "What is your name?"}
        {bind [user-name = {read-str}]
              [stringTurn = {(turn) => {if {equal? 0 turn}
                                          "Rock"
                                          {if {equal? 1 turn}
                                              "Paper"
                                              {if {equal? 2 turn}
                                                  "Sissor"
                                                  {error}}}}}]
              [setWinner = {(turn) => {if {equal? 0 turn}
                                          "Tie"
                                          {if {equal? 1 turn}
                                              "You Win!"
                                              "You Lose!"}}}]
              
              {seq
               {println "0 = Rock, 1 = Paper, and 2 = Sissor"}
               {println {++ user-name" move: "}}
               {bind [usr-move = {read-num}]
                    {seq
                     {println {++ user-name", you chose "
                                  {stringTurn usr-move} ". Now its my move!"}
                              }
                     {println{++ "I choose.... Rock!\n" {setWinner usr-move}}}
                     }}}}})
(top-interp example-program)

#|Example run
Lets play a game! 
Rock-Paper-Scissors
What is your name?
>Tron
0 = Rock, 1 = Paper, and 2 = Sissor
Tron move: 
>1
Tron, you chose Paper. Now its my move!
I choose.... Rock!
You Win!
|#

;###################################################################################################
;                                       Top-interp Program (test)
;###################################################################################################
(check-equal? (top-interp 3) "3")
(check-equal? (top-interp 'false) "false")
(check-equal? (top-interp "a") "\"a\"")
(check-equal? (top-interp '{<= 3 4}) "true")
(check-equal? (top-interp '{equal? 0 0}) "true")
(check-equal? (top-interp '{bind [test = {equal? 0 0}]
                   [then = 3]
                   [else = 4]
                   {if test then else}}) "3")
(check-equal? (top-interp '{{(f1) => {f1 {equal? 0 0} 2 5}}
                            {(test then else) => {if test then else}}}) "2")
(check-equal? (top-interp '{{(main) => {{(f1) => {f1 {equal? 0 0} 2 5}}
                                        {(test then else) => {if test then else}}}}
                            {(f) => {f}}}) "2")
(check-exn #px"Invalid_Arity"
           (λ ()
              (top-interp '{{(x) => (+ x 2)} 3 4 5})))

(check-exn #px"#<primop>"
           (λ ()(top-interp '(+ 4 (error "1234")))))

;###################################################################################################
;                                       Tests
;###################################################################################################
;Vars only used for testing. 
(define nc0 (NumC 0))
(define nc1 (NumC 1))
(define nc2 (NumC 2))
(define nc3 (NumC 3))
(define nc4 (NumC 4))
(define nc5 (NumC 5))
(define nc6 (NumC 6))
(define nc7 (NumC 7))
(define nc8 (NumC 8))
(define nc9 (NumC 9))
(define idca (IdC '+))
(define idcs (IdC '-))
(define idcm (IdC '*))
(define idcd (IdC '/))
(define idcle (IdC '<=))
(define nv0 (NumV 0))
(define nv1 (NumV 1))
(define nv2 (NumV 2))
(define nv3 (NumV 3))
(define nv4 (NumV 4))
(define nv5 (NumV 5))
(define nv6 (NumV 6))
(define nv7 (NumV 7))
(define nv8 (NumV 8))
(define nv9 (NumV 9))
(define bvt (BoolV #t))
(define bvf (BoolV #f))
(define pva (PrimV '+))
(define pvs (PrimV '-))
(define pvd (PrimV '/))
(define pvm (PrimV '*))
(define pvle (PrimV '<=))
(define cv1 (CloV '(c1) nc1 top-env))
(define cv2 (CloV '(c2) nc2 top-env))
(define cv3 (CloV '(c3) nc3 top-env))

;-----------------------------Interface functions--------------------------;
;parse (Sexp -> Expr)
(check-equal? (parse 2) nc2)
(check-equal? (parse 'f) (IdC 'f))
(check-equal? (parse "string") (StrC "string"))
(check-equal? (parse '{if (<= 2 4) 2 4}) (IfC (AppC idcle (list nc2 nc4)) nc2 nc4))
(check-equal? (parse '{bind
                       [x = 1]
                       [y = 8]
                       {+ x y}}) ;local binding {{(x) => {+ x 1}} 2} == {bind [x = 1]{+ x 1}}
              (AppC (LamC '(x y) (AppC idca (list (IdC 'x)(IdC 'y)))) (list nc1 nc8)))
(check-equal? (parse '{(x) => {+ x 1}})
              (LamC '(x) (AppC idca (list (IdC 'x) nc1))))
(check-equal? (parse '{() => 5})
              (LamC '() nc5))
(check-equal? (parse '{+ 1 2}) (AppC idca (list nc1 nc2)))
(check-equal? (parse '{- 1 2}) (AppC idcs (list nc1 nc2)))
(check-equal? (parse '{* 1 2}) (AppC idcm (list nc1 nc2)))
(check-equal? (parse '{/ 1 2}) (AppC idcd (list nc1 nc2)))
(check-equal? (parse '{/ 1 0}) (AppC idcd (list nc1 nc0))) ;div_by_zero is in interp
(check-exn #px"Invalid_ID"
           (λ ()
             (parse 'if)))
(check-exn #px"Dups_Found"
           (λ ()
             (parse '((x x) => 3))))

;interp 
(check-equal? (interp (parse 2) top-env) nv2)
(check-equal? (interp idca top-env) pva)
(check-equal? (interp (StrC "string") top-env) (StrV "string"))
(check-equal? (interp (AppC idca (list nc1 nc2)) top-env) nv3) ;primv
(check-equal? (interp (AppC idcs (list nc2 nc1)) top-env) nv1) ;primv
(check-equal? (interp (AppC idcm (list nc1 nc2)) top-env) nv2) ;primv
(check-equal? (interp (AppC idcd (list nc4 nc2)) top-env) nv2) ;primv
(check-equal? (interp (AppC (LamC '(x y) (AppC idca (list (IdC 'x)(IdC 'y)))) (list nc1 nc8))
                      top-env);clov
              nv9)
(check-equal? (interp (LamC '(x) (AppC idca (list (IdC 'x) nc1))) top-env)
              (CloV '(x) (AppC idca (list (IdC 'x) nc1)) top-env))
(check-equal? (interp (IfC (AppC idcle (list nc7 nc8)) nc7 nc8) top-env) nv7)
(check-equal? (interp (IfC (AppC idcle (list nc8 nc7)) nc7 nc8) top-env) nv8)
(check-exn #px"Invalid_AppC"
           (λ ()
             (interp (AppC (IdC 'invapp) '()) (cons (Bind 'invapp (NumV 3)) top-env))))
(check-exn #px"Undefined_Sym"
           (λ ()
             (interp (AppC (IdC 'foo) '()) top-env)))
(check-exn #px"Div_By_Zero"
           (λ ()
             (interp (AppC idcd (list nc4 nc0)) top-env)))
(check-exn #px"Invalid_Cond"
           (λ ()
             (interp (IfC (AppC idca (list nc8 nc7)) nc7 nc8) top-env)))

;-----------------------------Primitive Function Tests--------------------------;
;p-arth
(check-equal? (p-arith '+ (list nv3 nv4)) nv7)
(check-equal? (p-arith '- (list nv4 nv3)) nv1)
(check-equal? (p-arith '/ (list nv4 nv2)) nv2)
(check-equal? (p-arith '* (list nv2 nv3)) nv6)
(check-equal? (p-arith '<= (list nv3 nv4)) bvt)
(check-equal? (p-arith '<= (list nv4 nv3)) bvf)
(check-exn #px"Invalid_Value"
           (λ()(p-arith '+ (list nv3 bvt))))
(check-exn #px"Invalid_Value"
           (λ()(p-arith '- (list nv3 bvt))))
(check-exn #px"Invalid_Value"
           (λ()(p-arith '* (list nv3 bvt))))
(check-exn #px"Invalid_Value"
           (λ()(p-arith '/ (list nv3 bvt))))
(check-exn #px"Div_By_Zero"
           (λ()(p-arith '/ (list nv3 nv0))))
(check-exn #px"Invalid_Value"
           (λ()(p-arith '<= (list nv3 bvt))))
(check-exn #px"Invalid_Arity"
           (λ()(p-arith '+ (list nv3 nv9 nv7))))

;p-equal
(check-equal? (p-equal (list nv1 nv1)) bvt)
(check-equal? (p-equal (list nv1 nv2)) bvf)
(check-equal? (p-equal (list cv1 nv1)) bvf)
(check-equal? (p-equal (list cv1 cv1)) bvf)
(check-equal? (p-equal (list cv1 cv2)) bvf)
(check-equal? (p-equal (list pva pva)) bvf)
(check-equal? (p-equal (list pva pvs)) bvf)
(check-equal? (p-equal (list cv1 pvs)) bvf)

;p-error
(check-exn #px"user-error"
           (λ()(p-error (StrV "user message"))))


;println
;(top-interp '{println "Print this to the screen."})
;(top-interp '{seq {println "a"}{println "b"}})
;(top-interp '{seq {println "a"}})
;(top-interp '{++ true false})
;-----------------------------Helper function Tests--------------------------;
;Serialize
(check-equal? (serialize nv1) "1")
(check-equal? (serialize bvt) "true")
(check-equal? (serialize bvf) "false")
(check-equal? (serialize (StrV "string")) "\"string\"")
(check-equal? (serialize cv1) "#<procedure>")
(check-equal? (serialize pva) "#<primop>")

;id?
(check-equal? (id? 'if) #f)
(check-equal? (id? '=>) #f)
(check-equal? (id? 'bind) #f)
(check-equal? (id? '=) #f)
(check-equal? (id? 'foo) #t)

;valid-id
(check-equal? (valid-id 'foo) 'foo)
(check-exn #px"Invalid_ID"
           (λ()(valid-id 'if)))
(check-exn #px"Invalid_ID"
           (λ()(valid-id '=>)))
(check-exn #px"Invalid_ID"
           (λ()(valid-id 'bind)))
(check-exn #px"Invalid_ID"
           (λ()(valid-id '=)))

;valid-nodups
(check-equal? (valid-nodups '(x y z)) #t)
(check-exn #px"Dups_Found"
           (λ()(valid-nodups '(x x))))
;;err
(check-exn #px"Invalid_ID"
           (λ()(err 'parse 'Invalid_ID "Not found")))
(check-exn #px"Invalid_ID"
           (λ()(err 'parse 'Invalid_ID "Not found, Given ~e" 'notthis)))
(check-exn #px"Invalid_ID"
           (λ()(err 'parse 'Invalid_ID "Not found, Given ~e ~e" "String A" "String b")))

;lookup
(check-equal? (lookup '+ top-env) pva)
(check-equal? (lookup '- top-env) pvs)
(check-equal? (lookup '* top-env) pvm)
(check-equal? (lookup '/ top-env) pvd)
(check-equal? (lookup '<= top-env) pvle)
(check-equal? (lookup 'true top-env) bvt)
(check-equal? (lookup 'false top-env) bvf)
(check-equal? (lookup 'equal? top-env) (PrimV 'equal?))
(check-equal? (lookup 'error top-env) (PrimV 'error))
(check-exn #px"Undefined_Sym"
              (λ()(lookup 'foo top-env)))

;bld-env
(define bind-a (Bind 'a (NumV 2)))
(check-equal? (bld-env '() top-env) top-env)
(check-equal? (bld-env (list bind-a) top-env) (cons bind-a top-env))

;extract-syms
(check-equal? (extract-syms '()) '())
(check-equal? (extract-syms '([a = 4][b = 5])) '(a b))

;extract-vals
(check-equal? (extract-vals '()) '())
(check-equal? (extract-vals '([a = 4][b = 5])) '(4 5))

(top-interp '{bind [a = 3]
                   {bind [b = a]
                         {equal? a b}}})