#lang racket

(require "semantics.rkt")
(provide search-valid? deduce-valid?)

; Returns a duplicate-free list of variables that occur in f.
(define (variables f)
  (remove-duplicates (remove* '(⊤ ⊥ ¬ ∧ ∨ → ↔) (flatten f))))

; Returns true (#t) if f is valid and false (#f) otherwise,
; by checking that every interpretation satisfies f.
(define (search-valid? f)
  (let all-sat? ([vars (variables f)] [I (assign)])
    (match vars
      [(list)
       (evaluate f I)]
      [(list v vs ...)
       (and (all-sat? vs (extend I v false))
            (all-sat? vs (extend I v true)))])))

; Returns true (#t) if f is valid and false (#f) otherwise,
; by checking that all branches of f's proof tree are
; closed (i.e., contain a contradiction).
(define (deduce-valid? f)
  (let all-closed? ([facts `((I ⊭ ,f))])    ; assumption
    (match facts
      [`(,gs ... (I ⊨ (¬ ,f1)) ,hs ...)     ; (1)
       (all-closed? `((I ⊭ ,f1) ,@gs ,@hs))]
      [`(,gs ... (I ⊭ (¬ ,f1)) ,hs ...)     ; (2)
       (all-closed? `((I ⊨ ,f1) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (∧ ,fs ...)) ,hs ...) ; (3)
       (all-closed? `(,@(for/list ([fi fs]) `(I ⊨ ,fi)) ,@gs ,@hs))]
      [`(,gs ... (I ⊭ (∧ ,fs ...)) ,hs ...) ; (4)
       (for/and ([fi fs])
         (all-closed? `((I ⊭ ,fi) ,@gs ,@hs)))]
      [`(,gs ... (I ⊨ (∨ ,fs ...)) ,hs ...) ; (5)
       (for/and ([fi fs])
         (all-closed? `((I ⊨ ,fi) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (∨ ,fs ...)) ,hs ...) ; (6)
       (all-closed? `(,@(for/list ([fi fs]) `(I ⊭ ,fi)) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (→ ,f1 ,f2)) ,hs ...) ; (7)
       (and (all-closed? `((I ⊭ ,f1) ,@gs ,@hs))
            (all-closed? `((I ⊨ ,f2) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (→ ,f1 ,f2)) ,hs ...) ; (8)
       (all-closed? `((I ⊨ ,f1) (I ⊭ ,f2) ,@gs ,@hs))]
      [`(,gs ... (I ⊨ (↔ ,f1 ,f2)) ,hs ...) ; (9)
       (and (all-closed? `((I ⊨ (∧ ,f1 ,f2)) ,@gs ,@hs))
            (all-closed? `((I ⊭ (∨ ,f1 ,f2)) ,@gs ,@hs)))]
      [`(,gs ... (I ⊭ (↔ ,f1 ,f2)) ,hs ...) ; (10)
       (and (all-closed? `((I ⊨ (∧ ,f1 (¬ ,f2))) ,@gs ,@hs))
            (all-closed? `((I ⊨ (∧ (¬ ,f1) ,f2)) ,@gs ,@hs)))]
      [(or `(,_ ... (I ⊨ ,fi) ,_ ... (I ⊭ ,fi) ,_ ...)
           `(,_ ... (I ⊭ ,fi) ,_ ... (I ⊨ ,fi) ,_ ...))
       (printf "Contradiction on ~a: ~a\n" fi facts)
       #t]
      [_
       (printf "Open branch: ~a\n" facts)
       #f])))