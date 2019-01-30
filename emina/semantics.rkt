#lang racket

(provide (all-defined-out))

; We represent interpretations using immutable hashes
; and define the following operations on them: 
; * (assign x0 v0 x1 v1 ...) creates an interpretation
;   that assigns the value vi to the variable xi.
; * (lookup I xi) returns the value of the variable xi.
; * (extends I xi vi) returns a new interpretation that
;   is like I, except that it binds xi to vi.
(define assign hash)
(define lookup hash-ref)
(define extend hash-set)

; Returns true (#t) if I ⊨ f and false (#f) if I ⊭ f.
(define (evaluate f I)
  (match f
    ['⊤ true]
    ['⊥ false]
    [(? symbol? xi) (lookup I xi)]
    [`(¬ ,f1)       (not (evaluate f1 I))]
    [`(∧ ,fs ...)   (for/and ([fi fs]) (evaluate fi I))]
    [`(∨ ,fs ...)   (for/or  ([fi fs]) (evaluate fi I))]
    [`(→ ,f1 ,f2)   (or (not (evaluate f1 I)) (evaluate f2 I))]
    [`(↔ ,f1 ,f2)   (equal? (evaluate f1 I) (evaluate f2 I))]))
