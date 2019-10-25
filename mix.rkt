#lang racket

(require "fc-int.rkt")
(require "live-var.rkt")

(provide (all-defined-out))

(define (init-vars-immutable vars vals)
  (if (equal? (length vars) (length vals))
      (let()
        (define dict #hash())
        (for ([val vals] [var vars])
          (set! dict (dict-set dict var `',val)))
        dict)
      (error "Length mismatch")))

(define (get-dynamic-labels program division)
  (define labels (mutable-set (caadr program)))
  (for* ([bb (cdr program)]
        [command (cdr bb)]
        #:when (and (equal? (car command) `if) (not (static? (cadr command) division))))
    (set-add! labels (cadddr command))
    (set-add! labels (list-ref command 5)))
  (for/list ([label labels]) label))

(define (get-labels program)
  (for/list ([bb (cdr program)]) (car bb)))

(define (add-unmarked x xs marks)
  (if (set-member? marks x) xs (cons x xs)))

(define (remove-dead vs live)
  (for/hash ([(k v) (in-hash vs)]
             #:when (set-member? live k))
    (values k v)))

(define (static? expr division)
  (match expr
    [(list-rest e es) (and (static? e division) (static? es division))]
    [`,e (not (set-member? division e))]))

(define mix
  `((read program division vs0)
    
    (init (:= pp0 (caadr program))
          (:= pending `((,pp0 ,(init-vars-immutable (car vs0) (cadr vs0)))))
          (:= marked `(,(car pending)))
          (:= residual `(,(cons `read (set-subtract (cdar program) (car vs0)))))
          (:= live-vars (get-live-vars program))
          (goto pending-cond))

    (pending-cond (if (empty? pending) goto pending-end goto pending-body))
    (pending-body (:= point (car pending))
                  (:= pending (cdr pending))
                  (:= pp (car point))
                  (:= vs (cadr point))
                  (:= code `(,point))
                  (:= labels (get-dynamic-labels program division))
                  (goto lb-check))

        (lb-check (if (empty? labels) goto lb-error goto pps-cond))
        (pps-cond (:= pps (car labels))
                  (:= labels (cdr labels))
                  (if (equal? pp pps) goto pps-end goto lb-check))
        (pps-end  (:= bb (dict-ref program pps))
                  (goto bb-cond))
        
        (bb-cond (if (empty? bb) goto bb-end goto bb-body))
        (bb-body (:= command (car bb))
                 (:= bb (cdr bb))
                 (:= type (car command))
                 (goto check-assign))

            (check-assign (if (equal? type `:=)     goto do-assign goto check-if))
            (check-if     (if (equal? type `if)     goto do-if     goto check-goto))
            (check-goto   (if (equal? type `goto)   goto do-goto   goto check-return))
            (check-return (if (equal? type `return) goto do-return goto error))

            (do-assign (:= var (cadr command))
                       (:= expr (caddr command))
                       (if (static? var division) goto assign goto add-assign))
                (assign (:= vs (dict-set vs var `',(int-expr expr vs)))
                        (goto bb-cond))
                (add-assign (:= code (cons `(:= ,var ,(subst expr vs)) code))
                            (goto bb-cond))

            (do-if (:= expr (cadr command))
                   (:= then (cadddr command))
                   (:= else (list-ref command 5))
                   (if (static? expr division) goto goto-block goto add-if))
                (goto-block (if (int-expr expr vs) goto goto-then goto goto-else))
                    (goto-then (:= bb (dict-ref program then))
                               (goto bb-cond))
                    (goto-else (:= bb (dict-ref program else))
                               (goto bb-cond))
                (add-if (:= live-then (remove-dead vs (dict-ref live-vars then)))
                        (:= pending (add-unmarked `(,then ,live-then) pending marked))
                        (:= marked (cons `(,then ,live-then) marked))
                        (:= live-else (remove-dead vs (dict-ref live-vars else)))
                        (:= pending (add-unmarked `(,else ,live-else) pending marked))
                        (:= marked (cons `(,else ,live-else) marked))
                        (:= code (cons `(if ,(subst expr vs) goto (,then ,live-then) goto (,else ,live-else)) code))
                        (goto bb-cond))

            (do-goto (:= bb (dict-ref program (cadr command)))
                     (goto bb-cond))

            (do-return (:= code (cons `(return ,(subst (cadr command) vs)) code))
                       (goto bb-cond))

            (error (return (string-append "Undefined instruction: " (~a type))))
            (lb-error (return "Something is wrong"))

        (bb-end (:= residual (cons (reverse code) residual))
                ;(:= q (println (length marked)))
                (goto pending-cond))

    (pending-end (return (reverse residual)))))