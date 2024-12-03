#lang typed/racket

(require typed/rackunit)

; ExprC
; Represents an expression in our language
(define-type ExprC (U numC idC strC ifC lamC appC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([n : Symbol]) #:transparent)
(struct strC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

; BindC
(struct bindC ([name : Symbol] [val : Value]) #:transparent)

; Environment
; Represents an environment with bindings of variable names to values
(define-type EnvC (Listof bindC))
(define extend-env cons) ; Extend the environment

; Value
; Represents the different types of values in our language
(define-type Value (U numV boolV strV closV primV))
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : ExprC] [env : EnvC]) #:transparent)
(struct primV ([op : Symbol]) #:transparent)

; top-env : EnvC
; Defines the top-level environment with primitive bindings
(: top-env EnvC)
(define top-env
  (list
   (bindC 'true (boolV #true))
   (bindC 'false (boolV #false))
   (bindC '+ (primV '+))
   (bindC '- (primV '-))
   (bindC '* (primV '*))
   (bindC '/ (primV '/))
   (bindC '<= (primV '<=))
   (bindC 'equal? (primV 'equal?))
   (bindC 'error (primV 'error))
   (bindC 'println (primV 'println))
   (bindC 'read-num (primV 'read-num))
   (bindC 'read-str (primV 'read-str))
   (bindC 'seq (primV 'seq))
   (bindC '++ (primV '++))))

; interp : ExprC EnvC -> Value
; Evaluates an ExprC expression using the given environment
(: interp (ExprC EnvC -> Value))
(define (interp [exp : ExprC] [env : EnvC]) : Value
  (match exp
    [(numC n) (numV n)] ; numV
    [(idC n) (lookup n env)] ; idC
    [(strC s) (strV s)] ; strV
    [(ifC test then else) (if-eval test then else env)] ;ifC
    [(lamC params body) (closV params body env)] ; closV
    [(appC fun args) ; appC
     (define fval (interp fun env))
     (define values (map (lambda ([arg : ExprC]) : Value (interp arg env)) args))
     (match fval
       [(closV params body env-clos)
        (if (= (length params) (length values))
            (interp body (extend-all params values env-clos))
            (error 'interp
                   "AAQZ: Incorrect number of arguments, expected ~a, got ~a" (length params) (length values)))]
       [(primV op)
        (cond
          [(equal? op 'seq) (seq values)]
          [(equal? op 'read-num) (read-num)]
          [(equal? op 'read-str) (read-str)]
          [(equal? op '++) (++-handler values)]
          [(and (equal? (length values) 1) (equal? op 'println))
           (println-handler (first values))]
          [(and (equal? (length values) 1) (equal? op 'error))
           (err-handler (first values))]
          [(and (equal? (length values) 2) (valid-op? op))
           (match values
             [(list l r) (solve op l r)])]
          [else (error 'interp
                       "AAQZ: Incorrect number of arguments for operator, expected 2, got ~a" (length values))])
        ]
       [else (error 'interp
                    "AAQZ: invalid Syntax, got ~e" fval)])]))

; parse : Sexp -> ExprC
; Parses an S-expression into an ExprC
(: parse (Sexp -> ExprC))
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (numC n)] ; numC
    [(and (? valid-id? sym) (? symbol? sym)) (idC sym)] ; idC
    [(? string? str) (strC str)] ;strC
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))] ;ifC
    [(list 'bind clauses ... app) ; bind into lamC
     (match clauses
       ['() (appC (lamC '() (parse app)) '())]
       [(list (list (and (? valid-id? id) (? symbol? id)) '= expr))
        (appC (lamC (list id) (parse app)) (list (parse (cast expr Sexp))))]
       [(list (list (and (? valid-id? id) (? symbol? id)) '= expr) rest-clauses ...)
        (define ids (cons id (map (lambda (clause) (match clause
                                                      [(list (and (? valid-id? id) (? symbol? id)) '= expr) id]))
                                                  rest-clauses)))
        (define exprs (cons (parse (cast expr Sexp))
                            (map (lambda (clause)
                                   (match clause
                                     [(list (and (? valid-id? id) (? symbol? id)) '= expr) (parse (cast expr Sexp))]))
                                 rest-clauses)))
        (cond
          [(and (valid-ids? ids) (not (duplicates? ids))
                (and (equal? (length ids) (length exprs))))
           (appC (lamC ids (parse app)) exprs)]
          [else (error 'parse "AAQZ: invalid syntax, got ~e" ids)])]
       [else (error 'parse "AAQZ: invalid binds, ~e" clauses)])]
    [(list (list param ...) '=> body)  ; lamC
     (cond
       [(or (not (valid-ids? param)) (duplicates? param))
        (error 'parse "AAQZ: invalid syntax, got ~e" param)]
       [else
        (lamC (cast param (Listof Symbol)) (parse body))])]
    [(list fun args ...) ; appC
     (appC (parse fun) (map parse args))]
    [else (error 'parse "AAQZ: s-expression format is incorrect, got ~e" s)]))

; top-interp
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

; println-handler : Value -> Value
; Takes a value and prints it as a string followed by a newline.
; If the input is a string value, it prints it and returns a true boolean value.
; Otherwise, returns false without printing.
(define (println-handler [s : Value]) : Value
  (match s
    [(strV a) (begin (printf "~a" a) (newline) (boolV #t))]
    [_ (boolV #f)]))

; read-num : -> Value
; Prompts the user for numeric input by displaying "> ".
; Reads and converts the input into a number if possible and returns it as a numV.
; If the input is not a valid number, it raises an error with an informative message.
(define (read-num) : Value
  (display "> ")
  (define input (read-line))
  (define num (string->number (cast input String)))
  (cond
    [(and num (real? num)) (numV num)]
    [else (error 'read-num "AAQZ: input provided is not a number, got ~e" input)]))

; read-str : -> Value
; Prompts the user for string input by displaying "> ".
; Reads the input and returns it as a strV without the newline.
; If the input is not a valid string, it raises an error.
(define (read-str) : Value
  (display "> ")
  (define input (read-line))
  (match input
    [(? string? s) (strV s)]))

; seq-helper : (Listof Value) Value -> Value
; Helper function for seq that iterates through a list of values.
; Returns the last value in the list or the initial value if the list is empty.
(define (seq-helper [v : (Listof Value)] [last : Value]) : Value
  (match v
    ['() last]
    [(cons l r)
     (seq-helper r l)]))

; seq : (Listof Value) -> Value
; Evaluates a sequence of expressions in order and returns the result of the last expression.
; If the list of expressions is empty, it returns a default false boolean value.
(define (seq [v : (Listof Value)]) : Value
  (seq-helper v (boolV #f)))


; str-add helper function for ++-handler
(define (str-add [s : Value] [p : Value]) : Value
  (cond
    [(and (strV? s) (strV? p)) (strV (string-append (strV-s s) (strV-s p)))]
    [(and (strV? s) (not (strV? p))) (strV (string-append (strV-s s) (serialize p)))]
    [(and (not (strV? s)) (strV? p)) (strV (string-append (serialize s) (strV-s p)))]
    [(and (not (strV? s)) (not (strV? p))) (strV (string-append (serialize s) (serialize p)))]))

; ++-handler
(define (++-handler [v : (Listof Value)]) : Value
  (match v
    ['() (strV "")]
    [(cons l r) (str-add l (++-handler r))]))

; err-handler : Value -> Nothing
; Raises error and makes it serializable
(: err-handler (Value -> Nothing))
(define (err-handler [e : Value]) : Nothing
  (error 'err-handler "AAQZ: user-error ~e" (serialize e)))

; valid-id? : Any -> Boolean
; Returns true if the given value is a valid identifier (not a keyword)
(: valid-id? (Any -> Boolean))
(define (valid-id? [id : Any]) : Boolean
  (match id
    [(? real? id) #f]
    [(or 'if '=> 'bind '=) #f]
    [_ #t]))

; valid-ids? : Any -> Boolean
(: valid-ids? ((Listof Any) -> Boolean))
(define (valid-ids? [ids : (Listof Any)]) : Boolean
  (match ids
    ['() #t]
    [(cons l r) (if (valid-id? l) (valid-ids? r) #f)]))

; duplicates? : (Listof Symbol) -> Boolean
; Returns true if the list of symbols contains duplicates
(define (duplicates? [s : (Listof Any)]) : Boolean
  (match s
    ['() #f]
    [(cons l r) (if (not (member l r)) (duplicates? r) #t)]))

; extend-all : (Listof Symbol) (Listof Value) EnvC -> EnvC
; Extends an environment with multiple variable bindings 
(define (extend-all[params : (Listof Symbol)] [args : (Listof Value)] [env : EnvC]) : EnvC
  (match (list params args)
    [(list '() '()) env]
    [(list (cons p ps) (cons a as)) (extend-env (bindC p a) (extend-all ps as env))]
    [_ (error 'extend-all "Mismatched number of parameters and arguments.")]))

; valid-op? : Sexp -> Boolean
; Returns true if the given symbol is a valid arithmetic operation
(: valid-op? (Sexp -> Boolean))
(define (valid-op? [s : Sexp]) : Boolean
  (match s
    [(or '+ '- '* '/ '<= 'equal?) #t]
    [_ #f]))

; solve : Symbol Value Value -> Value
; Solves an arithmetic or comparison operation based on the given operator and operands
(: solve (Symbol Value Value -> Value))
(define (solve [op : Symbol] [l : Value] [r : Value]) : Value
  (match (list op l r)
    [(list '+ (? numV? l) (? numV? r)) (numV (+ (numV-n l) (numV-n r)))]  ; Addition
    [(list '- (? numV? l) (? numV? r)) (numV (- (numV-n l) (numV-n r)))]  ; Subtraction
    [(list '* (? numV? l) (? numV? r)) (numV (* (numV-n l) (numV-n r)))]  ; Multiplication
    [(list '/ (? numV? l) (? numV? r)) ; Division
     (cond
       [(= (numV-n r) 0) (error 'solve "AAQZ: cannot divide by 0, got ~e/~e" (numV-n l) (numV-n r))]
       [else (numV (/ (numV-n l) (numV-n r)))])]
    [(list '<= (? numV? l) (? numV? r)) (boolV (<= (numV-n l) (numV-n r)))] ; less than or equal too
    [(list 'equal? l r) ; equal?
     (cond
       [(or (closV? l) (closV? r) (primV? l) (primV? r)) (boolV #false)]
       [else (boolV (equal? l r))])]
    [else (error 'solve "AAQZ: Unsupported operation or type mismatch ~e" op)]))

; lookup : Symbol EnvC -> Value
; Looks up the value of a symbol in the given environment
(: lookup (Symbol EnvC -> Value))
(define (lookup [for : Symbol] [env : EnvC]) : Value
  (match env
    ['() (error 'lookup "AAQZ: name not found: ~e" for)]
    [(cons (bindC name val) r)
     (cond
       [(symbol=? for name) val]
       [else (lookup for r)])]))

; serialize : Value -> String
; Serializes a value into a string representation
(: serialize (Value -> String))
(define (serialize [input : Value]) : String
  (match input
    [(numV n) (~v n)]
    [(boolV b)
     (cond
       [(equal? b #true) "true"]
       [else "false"])]
    [(closV arg body env) (format "#<procedure>")]
    [(primV _) "#<primop>"]
    [(strV s) (~v s)]))

; if-eval : ExprC ExprC ExprC EnvC -> Value
; Evaluates an if expression by checking the test condition in the given environment.
; If the test evaluates to true, it evaluates and returns the 'then' expression.
; If the test evaluates to false, it evaluates and returns the 'else' expression.
; Throws an error if the test condition does not produce a boolean value.
(: if-eval (ExprC ExprC ExprC EnvC -> Value))
(define (if-eval [test : ExprC] [then : ExprC] [else : ExprC] [env : EnvC]) : Value
  (match (interp test env)
    [(boolV #true) (interp then env)]
    [(boolV #false) (interp else env)]
    [_ (error 'if-eval "AAQZ: test did not give a boolean, given ~e" test)]))