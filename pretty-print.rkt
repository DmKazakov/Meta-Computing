#lang racket

(provide pretty-print)

(define (pretty-print program)
  (define duplicate-cnt (make-hash))
  (define new-labels (make-hash))

  (define (relabel label)
    (when (not (hash-has-key? new-labels label))
      (let()
          (define orig-label (car label))
          (if (hash-has-key? duplicate-cnt orig-label)
              (dict-set! duplicate-cnt orig-label (+ (dict-ref duplicate-cnt orig-label) 1))
              (dict-set! duplicate-cnt orig-label 0))
          (define duplicates (dict-ref duplicate-cnt orig-label))
          (dict-set! new-labels label (string-append (~a orig-label) "_" (~a duplicates)))))
    (dict-ref new-labels label))

  (define (relabel-block bb)
    (cons (relabel (car bb))
          (for/list ([command (cdr bb)])
            (match command
              [`(if ,expr goto ,then goto ,else) `(if ,expr goto ,(relabel then) goto  ,(relabel else))]
              [`(goto ,label) `(goto ,(relabel label))]
              [_ command]))))

  (cons (car program)
        (for/list ([bb (cdr program)])
          (relabel-block bb))))