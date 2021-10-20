#lang racket

(provide (all-defined-out))

(define (head list)
  (if (empty? list) `() (car list)))

(define (tail list)
  (if (empty? list) `() (cdr list)))

(define (replace-head list head)
  (if (empty? list)
      (list head)
      (cons head (cdr list))))

(define tm-int
  `((read instrs right_tape)
    (init (:= next-instrs instrs)
          (:= left_tape `())
          (goto continue))
    
    (continue (if (empty? next-instrs) goto end goto next-instr))
    
    (next-instr  (:= instr (cdar next-instrs))
                 (:= type (car instr))
                 (:= next-instrs (cdr next-instrs))
                 (goto check-left))
    (check-left  (if (equal? type `left)  goto do-left  goto check-right))
    (check-right (if (equal? type `right) goto do-right goto check-write))
    (check-write (if (equal? type `write) goto do-write goto check-goto))
    (check-goto  (if (equal? type `goto)  goto do-goto  goto check-if))
    (check-if    (if (equal? type `if)    goto do-if    goto error))

    (do-left  (:= right_tape (cons (head left_tape) right_tape))
              (:= left_tape (tail left_tape))
              (goto continue))
    (do-right (:= left_tape (cons (head right_tape) left_tape))
              (:= right_tape (tail right_tape))
              (goto continue))
    (do-write (:= char (cadr instr))
              (:= right_tape (replace-head right_tape char))
              (goto continue))
    (do-goto  (:= instr-num (cadr instr))
              (goto jump))
    (do-if    (:= char (cadr instr))
              (:= instr-num (cadddr instr))
              (if (equal? char (head right_tape)) goto jump goto continue))

    (error (return (string-append "Undefined instruction: " (~a type))))
    (jump  (:= next-instrs (list-tail instrs instr-num))
           (goto next-instr))
    (end   (return right_tape))))