#! /usr/bin/env racket

#lang racket

(require "lambda.rkt")

(define (repl)
  (letrec ([input-loop
            (lambda ()
              (begin
                (display "> ")
                (let ([line (read-line)])
                  (if (string? line)
                      (lambda-eval (string->lambda line))
                      (begin
                        (displayln "Thanks for visiting!")
                        (exit))))
                (input-loop)))])
    (begin
      (displayln "Welcome to Lambda!")
      (input-loop))))

(repl)
