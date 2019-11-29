#lang racket

(require "fc-int.rkt")

(provide (all-defined-out))

(define fc-int
  `((read program vals)
    (init3 (:= vars (cdar program))
           (:= scope (init-vars vars vals))
           (:= blocks (init-blocks (cdr program)))
           (:= label (caadr program))
           (goto int-block3))

    (int-block3 (:= stmts (dict-ref blocks label))
                (goto int-stmt3))

    (int-stmt3 (:= stmt (car stmts))
               (:= stmts (cdr stmts))
               (:= type (car stmt))
               (if (equal? type `:=) goto int-assign3 goto check-if3))

    (int-assign3 (:= var (cadr stmt))
                 (:= expr (eval (subst (caddr stmt) scope)))
                 (:= q (dict-set! scope var `',expr))
                 (goto int-stmt3))

    (check-if3    (if (equal? type `if)     goto int-if3     goto check-goto3))
    (check-goto3  (if (equal? type `goto)   goto int-goto3   goto check-return3))
    (check-return3 (if (equal? type `return) goto int-return3 goto error3))
    (error3 (return (string-append "Undefined stmt type: " (~a type))))

    (int-if3 (:= cond (eval (subst (cadr stmt) scope)))
             (:= then (cadddr stmt))
             (:= else (list-ref stmt 5))
             (if cond goto goto-then3 goto goto-else3))
    (goto-then3 (:= label then)
                (goto int-block3))
    (goto-else3 (:= label else)
                (goto int-block3))

    (int-goto3 (:= label (cadr stmt))
               (goto int-block3))

    (int-return3 (return (eval (subst (cadr stmt) scope))))))