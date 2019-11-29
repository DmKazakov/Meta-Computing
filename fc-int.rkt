#lang racket

(provide (all-defined-out))

(define (init-vars vars vals)
  (if (equal? (length vars) (length vals))
      (let()
        (define dict (make-hash))
        (for ([val vals] [var vars])
          (dict-set! dict var `',val))
        dict)
      (error "Length mismatch!")))

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
       (int-jump jump scope blocks))]
    [_ (error (string-append "Empty block: " (~a label)))]))

(define (int-assignment assignment scope)
  (match assignment
    [`(:= ,var ,expr) (let()
                          ;(cond
                          ;  [(eq? var 'code) (displayln expr)])
                        (dict-set! scope var `',(int-expr expr scope)))]
    [_ (error (string-append "Not an assignment: " (~a assignment)))]))

(define (int-expr expr scope)
  (define e (subst expr scope))
  (with-handlers ([exn:fail? (lambda (exn) ((printf "~a\n~a\n~a\n~a\n\n" exn expr e scope)
                                            (error (~a expr))))])
    (eval e)))

(define (subst expr scope)
  ;(printf "subst: ~a\n" expr)
  (match expr
    [(list-rest 'quasiquote es) `(,(car expr) . ,(subst-quasiquote es scope))]
    [(list-rest 'quote es) expr]
    [(list-rest e es) `(,(subst e scope) . ,(subst es scope))]
    [e (if (dict-has-key? scope e) (dict-ref scope e) e)]))

(define (subst-quasiquote expr scope)
  ;(printf "subst-quasiquote ~a\n" expr)
  (match expr
    [(list-rest 'unquote es) `(,(car expr) . ,(subst-quasiquote-unquote es scope))]
    [(list-rest e es) `(,(subst-quasiquote e scope) . ,(subst-quasiquote es scope))]
    [e e]))

(define (subst-quasiquote-unquote expr scope)
  ;(printf "subst-quasiquote-unquote ~a\n" expr)
  (match expr
    [(list-rest 'quasiquote es) `(,(car expr) . ,(subst-quasiquote es scope))]
    [(list-rest 'quote es) `(,(car expr) . ,(subst-quasiquote es scope))]
    [(list-rest e es) `(,(subst-quasiquote-unquote e scope) . ,(subst-quasiquote-unquote es scope))]
    [e (if (dict-has-key? scope e) (dict-ref scope e) e)]))

(define (int-jump jump scope blocks)
  (match jump
    [`(goto ,label) (int-block label scope blocks)]
    [`(if ,cond goto ,then goto ,else) (int-block (if (int-expr cond scope) then else) scope blocks)]
    [`(return ,expr) (int-expr expr scope)]))