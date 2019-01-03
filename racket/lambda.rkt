#! /usr/bin/env racket

#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens lambda-tokens (ID))
(define-empty-tokens lambda-empty-tokens (\\ \. \( \) := EOF))
(define-lex-abbrevs
  (digit (char-range #\0 #\9)))

(define lambda-lexer
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space #\newline)
    (lambda-lexer input-port)]
   [(:or "\\" "." "(" ")" ":=")
    (string->symbol lexeme)]
   [(:: (:or #\_ (:: alphabetic))
        (:* (:or #\_ (:: alphabetic) (:: digit))))
    (token-ID lexeme)]))

(define lambda-parser
  (parser
   (start statement)
   (end EOF)
   (tokens lambda-tokens lambda-empty-tokens)
   (error (lambda (ok? name value)
            (printf "Something failed :( ~a ~a\n" name value)))

   (grammar
    (statement   [(ID := abstraction)  (list 'assignment $1 $3)]
                 [(application)                              $1])
    (abstraction [(\\ ID \. abstraction)  (list 'abstraction $2 $4)]
                 [(application)                                  $1])
    (application [(application atomic)  (list 'application $1 $2)]
                 [(atomic)                                     $1])
    (atomic [(ID)                 $1]
            [(\( abstraction \))  $2]))))

(define (interpret str)
  (let* ([port (open-input-string str)]
         [result (lambda-parser (lambda () (lambda-lexer port)))])
    (displayln result)))

(displayln "Welcome to Lambda!")
(for ([i (in-naturals)])
  (display "> ")
  (let ([line (read-line)])
    (if (string? line)
        (interpret line)
        ((display line)
         (exit)))))
