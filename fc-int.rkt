#lang racket

(provide (all-defined-out))

(define (init-vars vars vals)
  (if (equal? (length vars) (length vals))
      (let()
        (define dict (make-hash))
        (for ([val vals] [var vars])
          (dict-set! dict var `',val))
        dict)
      (error "Length mismatch")))

(define (init-blocks blocks)
  (define dict (make-hash))
  (for ([block blocks])
    (dict-set! dict (car block) (cdr block)))
  dict)

(define (int prog vals)
  (define vars (cdar prog))
  (define scope (init-vars vars vals))
  (define blocks (init-blocks (cdr prog)))
  (int-block (caadr prog) scope blocks))

(define (int-block label scope blocks)
  (define statements (dict-ref blocks label))
  (match statements
    [(list assignments ... jump)
     (let()
       (for ([assignment assignments])
         (int-assignment assignment scope))
       (int-jump jump scope blocks))]))

(define (int-assignment assignment scope)
  (match assignment
    [`(:= ,var ,expr) (dict-set! scope var `',(int-expr expr scope))]))

(define (int-expr expr scope)
  (define e (subst expr scope))
  (eval e))

(define (subst expr scope)
  (match expr
    [(list-rest e es) `(,(subst e scope) . ,(subst es scope))]
    [e (if (dict-has-key? scope e) (dict-ref scope e) e)]))

(define (int-jump jump scope blocks)
  (match jump
    [`(goto ,label) (int-block label scope blocks)]
    [`(if ,cond goto ,then goto ,else) (int-block (if (int-expr cond scope) then else) scope blocks)]
    [`(return ,expr) (int-expr expr scope)]))