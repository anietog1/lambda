#! /usr/bin/env racket
#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens lambda-tokens (ID))
(define-empty-tokens lambda-empty-tokens (\\ \. \( \) EOF))
(define-lex-abbrevs
  (letter (char-range "a" "z")))

(define lambda-lexer
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)
    (lambda-lexer input-port)]
   [(:or "\\" "." "(" ")")
    (string->symbol lexeme)]
   [(:: letter)
    (token-ID lexeme)]))

(define lambda-parser
  (parser
   (start expression)
   (end EOF)
   (tokens lambda-tokens lambda-empty-tokens)
   (error (lambda (ok? name value) (printf "Couldn't parse: ~a\n" name)))

   (grammar
    (expression [(\\ variable-list \. expression) (list $2 $4)]
                [(application-term)                         (list $1)])
    (variable-list [(variable-list ID) (append $1 (list $2))]
                   [(ID)                           (list $1)])
    (application-term [(application-term item) (append $1 (list $2))]
                      [(item)                             (list $1)])
    (item [(ID)               (list $1)]
          [(\( expression \)) (list $2)]))))

(define (interpret str)
  (let* ([port (open-input-string str)]
         [result (lambda-parser (lambda () (lambda-lexer port)))])
    (displayln result)))

(displayln "Welcome to lambda.rkt!")
(for ([i (in-naturals)])
  (display "> ")
  (interpret (read-line)))
