#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(struct binding (name e) #:transparent)

(struct abstraction (arg body) #:transparent)
(struct application (e1 e2) #:transparent)
(struct variable (name) #:transparent)

(struct closure (arg body env) #:transparent)
(struct inspection (e) #:transparent)

(define-tokens lambda-tokens (ID))
(define-empty-tokens lambda-empty-tokens (\\ \. \( \) = \; \? EOF))
(define-lex-abbrevs (digit (char-range #\0 #\9)))

(define lambda-lexer
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)
    (lambda-lexer input-port)]
   [(:or "\\" "." "(" ")" "=" ";" "?")
    (string->symbol lexeme)]
   [(:: (:or #\_ (:: alphabetic))
        (:* (:or #\_ (:: alphabetic) (:: digit))))
    (token-ID lexeme)]))

(define lambda-parser
  (parser
   (start statement-list)
   (end EOF)
   (tokens lambda-tokens lambda-empty-tokens)
   (error (lambda (ok? name value)
            (printf "Something failed :( ~a ~a\n" name value)))
   (grammar
    (statement-list
     [(statement \; statement-list) (cons $1 $3)]
     [() null])
    (statement
     [(ID = abstraction) (binding $1 $3)]
     [(\? abstraction) (inspection $2)])
    (abstraction
     [(\\ ID \. abstraction) (abstraction $2 $4)]
     [(application) $1])
    (application
     [(application atomic) (application $1 $2)]
     [(atomic) $1])
    (atomic
     [(ID) (variable $1)]
     [(\( abstraction \)) $2]))))

(define (program->lambda program)
  (let* ([port (open-input-string program)]
         [lexer (lambda () (lambda-lexer port))])
    (lambda-parser lexer)))

(define (string->lambda s)
  (let ([xs (program->lambda s)])
    (if (= (length xs) 1)
        (car xs)
        (error "Only 1 expression was expected in string"))))

(define (lambda->string e)
  (cond [(binding? e)
         (let ([name (binding-name e)]
               [e (binding-e e)])
           (string-append name "=" (lambda->string e) ";"))]
        [(abstraction? e)
         (let ([arg (abstraction-arg e)]
               [body (abstraction-body e)])
           (string-append "(" "\\" arg "." (lambda->string body) ")"))]
        [(application? e)
         (let ([e1 (application-e1 e)]
                 [e2 (application-e2 e)])
           (string-append
            "(" (lambda->string e1) " " (lambda->string e2) ")"))]
        [(variable? e)
         (variable-name e)]
        [(closure? e)
         (let ([arg (closure-arg e)]
               [body (closure-body e)])
           (string-append "(" "\\" arg "." (lambda->string body) ")"))]
        [(inspection? e)
         (let ([e (inspection-e e)])
           (string-append "?" (lambda->string e)))]
        [#t (error "The given expression is not part of the lambda language definition")]))

(define (lambda->program program)
  (foldr string-append "" (map lambda->string program)))

(define global-env null)

(define (lookup-variable name env)
  (cdr (assoc name env)))

(define (eval-under-env e env)
  (cond [(binding? e)
         (let ([name (binding-name e)]
               [e (eval-under-env (binding-e e) env)])
           (begin
             (set! global-env (cons (cons name e) global-env))
             e))]
        [(abstraction? e)
         (let ([arg (abstraction-arg e)]
               [body (abstraction-body e)])
           (closure arg body env))]
        [(application? e)
         (let ([e1 (eval-under-env (application-e1 e) env)]
               [e2 (eval-under-env (application-e2 e) env)])
           (if (closure? e1)
               (let* ([arg (closure-arg e1)]
                      [env (cons (cons arg e2) env)])
                 (eval-under-env e1 env))
               (error "Can't call application with non-closure")))]
        [(variable? e)
         (let ([name (variable-name e)])
           (lookup-variable name env))]
        [(closure? e) e]
        [(inspection? e)
         (let ([e (inspection-e e)])
           (begin
             (println (lambda->string (eval-under-env e env)))
             e))]
        [#t (error "The given expression is not part of the lambda language definition")]))

(define (execute-program program)
  (begin
    (set! global-env null)
    (map (lambda (e) (eval-under-env e global-env)) program)
    global-env))
