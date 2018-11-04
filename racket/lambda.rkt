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
   (start expr)
   (end EOF)
   (tokens lambda-tokens lambda-empty-tokens)
   (error (lambda (ok? name value) (printf "Couldn't parse: ~a\n" name)))

   (precs (right \\)
          (left \.))

   (grammar
    (expr [(\( ID \))            (list $2)] ;; variable
          [(\( \\ ID \. expr \)) (list $3 $5)] ;; abstraction
          [(\( expr expr \))     (list $2 $3)])))) ;; application

(define (interpret str)
  (let* ([port (open-input-string str)]
         [result (lambda-parser (lambda () (lambda-lexer port)))])
    (displayln result)))

(displayln "Welcome to lambda.rkt!")
(for ([i (in-naturals)])
  (display "> ")
  (interpret (read-line)))
