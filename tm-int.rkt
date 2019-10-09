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
    (init2 (:= next-instrs instrs)
           (:= left_tape `())
           (goto continue2))
    
    (continue2 (if (empty? next-instrs) goto end2 goto next-instr2))
    
    (next-instr2  (:= instr (cdar next-instrs))
                  (:= type (car instr))
                  (:= next-instrs (cdr next-instrs))
                  (goto check-left2))
    (check-left2  (if (equal? type `left)  goto do-left2  goto check-right2))
    (check-right2 (if (equal? type `right) goto do-right2 goto check-write2))
    (check-write2 (if (equal? type `write) goto do-write2 goto check-goto2))
    (check-goto2  (if (equal? type `goto)  goto do-goto2  goto check-if2))
    (check-if2    (if (equal? type `if)    goto do-if2    goto error2))

    (do-left2  (:= right_tape (cons (head left_tape) right_tape))
               (:= left_tape (tail left_tape))
               (goto continue2))
    (do-right2 (:= left_tape (cons (head right_tape) left_tape))
               (:= right_tape (tail right_tape))
               (goto continue2))
    (do-write2 (:= char (cadr instr))
               (:= right_tape (replace-head right_tape char))
               (goto continue2))
    (do-goto2  (:= instr-num (cadr instr))
               (goto jump2))
    (do-if2    (:= char (cadr instr))
               (:= instr-num (cadddr instr))
               (if (equal? char (head right_tape)) goto jump2 goto continue2))

    (error2 (return (string-append "Undefined instruction: " (~a type))))
    (jump2  (:= next-instrs (list-tail instrs instr-num))
            (goto next-instr2))
    (end2   (return right_tape))))