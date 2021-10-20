#lang racket

(require dyoo-while-loop)

(provide get-live-vars)

(define (get-live-vars program)
  (define block-vars (get-used-defined-vars program))
  (define marked (mutable-set))
  (define changed #t)

  (define (dfs label)
    (when (not (set-member? marked label))
      (set-add! marked label)
      (define used (mutable-set))
      (for ([next (caddr (dict-ref block-vars label))])
        (dfs next)
        (set-union! used (car (dict-ref block-vars next))))
      (define bv (dict-ref block-vars label))
      (set-subtract! used (cadr bv))
      (when (not (subset? used (car bv))) (set! changed #t))
      (set-union! (car bv) used)))

  (define first (caadr program))
  (while changed
         (set! changed #f)
         (set! marked (mutable-set))
         (dfs first))
  
  (for/hash ([(k v) (in-hash block-vars)]) (values k (car v))))

(define (get-used-defined-vars program)
  (define block-vars (make-hash))
  (for ([block (cdr program)])
    (dict-set! block-vars (car block) (get-block-used-defined-vars (cdr block))))
  block-vars)

(define (get-block-used-defined-vars block)
  (define used (mutable-set))
  (define defined (mutable-set))
  (define labels (mutable-set))

  (define (add-undefined expr)
    (for ([el (flatten expr)] #:when (not (set-member? defined el)))
      (set-add! used el)))
  
  (for ([command block])
    (match command
      [`(:= ,var ,expr) (let()
                          (add-undefined expr)
                          (set-add! defined var))]
      [`(if ,expr goto ,then goto ,else) (let()
                                           (add-undefined expr)
                                           (set-add! labels then)
                                           (set-add! labels else))]
      [`(goto ,label) (set-add! labels label)]
      [`(return ,expr) (add-undefined expr)]))
  `(,used ,defined ,labels))