#lang racket

(provide search-sat dpll read-cnf)

; The procedures search-sat and dpll assume that a CNF formula is
; given as a list of clauses, where each clause is a list of non-zero
; integers representing positive or negative literals.  

; If f is satisfiable, returns a model of f given as a list of
; integers (positive or negative literals). Otherwise returns false.
(define (search-sat f)
  (let search ([vars (variables f)] [I (list)])
    (if (evaluate f I)
        I
        (match vars
          [(list) #f]
          [(list v vs ...)
           (or (search vs (cons v I))
               (search vs (cons (- v) I)))]))))

; If f is satisfiable, returns a model of f given as a list of
; integers (positive or negative literals). Otherwise returns false.
(define (dpll f)
  (define g (bcp f))
  (match g
    [`((,lit) ...) lit]                         ; model
    [`(,_ ... () ,_ ...) #f]                    ; unsat
    [`((,assigned) ... (,lit ,_ ,_ ...) ,_ ...) ; search
     (let* ([undecided (drop g (length assigned))]
            [result    (or (dpll `((,lit) ,@undecided))
                           (dpll `((,(- lit)) ,@undecided)))])
       (and result `(,@assigned ,@result)))]))

; Applies BCP to f.
(define (bcp f)
  (match f
    [`(,ci ... (,lit) ,cj ...)
     `((,lit) ,@(bcp (unit-rule lit `(,@ci ,@cj))))]
    [_ f]))

; Applies the unit rule to f with respect to the given literal.
(define (unit-rule lit f)
  (define -lit (- lit))
  (for/list ([clause f] #:unless (member lit clause))
    (match clause
      [`(,li ... ,(== -lit) ,lj ...) `(,@li ,@lj)]
      [_ clause])))


; Returns the variables in a CNF formula f represented as a
; list of lists of integers.
(define (variables f)
  (remove-duplicates (map abs (flatten f))))

; Returns true iff I is a model of the CNF formula f
; represented as a list of lists of integers.
(define (evaluate f I)
  (for/and ([c f])
    (for/or ([l c] #:when (member l I)) #t)))

; Returns a list of lists representation of a CNF formula
; in the standard DIMACS format.
(define (read-cnf file)
  (call-with-input-file file
    (lambda (port)
      (filter-map
       (lambda (ln)
         (and (not (string-prefix? ln "c"))
              (not (string-prefix? ln "p"))
              (map string->number (drop-right (string-split ln) 1))))
       (port->lines port)))))