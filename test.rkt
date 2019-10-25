#lang racket

(require "fc-int.rkt")
(require "fc2fc-int.rkt")
(require "tm-int.rkt")
(require "mix.rkt")
(require "live-var.rkt")
(require "pretty-print.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))


(define tm-example '((0 if 0 goto 3)
                     (1 right)
                     (2 goto 0)
                     (3 write 1)))
#|
(define futamura1-tm (eval '(int mix `(,tm-int (right_tape left_tape) ((instrs) (,tm-example)))) ns))
(eval '(int futamura1-tm `((1 1 0 1 1 0 1))) ns)

(define futamura2-tm (eval '(int mix `(,mix
                                       (vs0 pending marked residual point pp vs code live-then live-else q)
                                       ((program division) (,tm-int (right_tape left_tape))))) ns))
(define target-tm (eval '(int futamura2-tm `(((instrs) (,tm-example)))) ns))
(eval '(int target-tm `((1 1 0 1 1 0 1))) ns)
|#
(define futamura3-tm (eval '(int mix `(,mix
                                       (vs0 pending marked residual point pp vs code live-then live-else q)
                                       ((program division) (,mix (vs0 pending marked residual point pp vs code live-then live-else q))))) ns))
(pretty-print futamura3-tm)
#|
(equal? futamura1-tm target-tm)


(define fc-example
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) goto found goto cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))

(define futamura1-fc (eval '(int mix `(,fc-int (vals scope q expr cond) ((program) (,fc-example)))) ns))
(eval '(int futamura1-fc `((b (a b c) (1 2 3)))) ns)

(define futamura2-fc (eval '(int mix `(,mix
                                       (vs0 pending marked residual point pp vs code live-then live-else q)
                                       ((program division) (,fc-int (vals scope q expr cond))))) ns))
(define target-fc (eval '(int futamura2-fc `(((program) (,fc-example)))) ns))
(eval '(int target-fc `((b (a b c) (1 2 3)))) ns)

(equal? futamura1-fc target-fc)

(pretty-print futamura1-fc)
|#


