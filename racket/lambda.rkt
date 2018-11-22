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
   (error (lambda (ok? name value) (printf "Something failed :(\n" name)))

   (grammar
    (expression [(\\ ID \. expression)  (list 'abstract $2 $4)]
                [(application-term)                         $1])
    (application-term [(application-term atomic-term)  (list 'apply $1 $2)]
                      [(atomic-term)                                    $1])
    (atomic-term [(ID)                $1]
                 [(\( expression \))  $2]))))

(define (interpret str)
  (let* ([port (open-input-string str)]
               [result (lambda-parser (lambda () (lambda-lexer port)))])
    (displayln result)))

(displayln "Welcome to lambda.rkt!")
(for ([i (in-naturals)])
     (display "> ")
     (let ([line (read-line)])
       (if (string? line)
           (interpret line)
         (exit))))
