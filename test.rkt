#lang racket

(require "fc-int.rkt")
(require "tm-int.rkt")
(require "mix.rkt")

(define tm-example '((0 if 0 goto 3)
                     (1 right)
                     (2 goto 0)
                     (3 write 1)))

;(define futamura1-tm (int mix `(,tm-int (right_tape left_tape) ((instrs) (,tm-example)))))
;(int futamura1-tm `((1 1 0 1 1 0 1)))