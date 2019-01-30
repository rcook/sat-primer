#lang racket

(require "normal-forms.rkt")
(provide tseitin)

; Converts f to an equisatisfiable formula in CNF
; using Tseitin's encoding.
(define (tseitin f)
  (cnf (auxiliaries f)))

; Returns a conjunction of formulas of the form
; (∧ a ... (↔ ai fi) ...), where a's are fresh
; auxiliary variables, and fi's are f's subformulas,
; with nested subexpressions replaced by their
; corresponding auxiliary variables.
(define (auxiliaries f)
  (define aux (make-hash))
  (let intro ([f f])
    (and (list? f)
         (not (hash-has-key? aux f))
         (hash-set! aux f (gensym 'a))
         (for-each intro (cdr f))))
  `(∧ ,(hash-ref aux f f)
      ,@(for/list ([fi↦a (sort (hash->list aux) symbol<? #:key cdr)])
          (match-define (cons `(,op ,fs ...) a) fi↦a)
          `(↔ ,a (,op ,@(for/list ([fj fs]) (hash-ref aux fj fj)))))))