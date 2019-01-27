#lang racket

(provide nnf dnf cnf)

; Converts f to NNF by first limiting operators
; to ∧,∨¬ and then pushing ¬ down to the leaves
; (literals) of the formula.
(define (nnf f)
 (nnf-neg (nnf-ops f)))

; Converts f to DNF by first converting it to NNF,
; then distributing ∧ over ∨, and finally flattening
; out nested conjunctions and disjunctions.
(define (dnf f)
  (unnest (distribute '∧ '∨ (nnf f))))

; Converts f to CNF as done for the DNF conversion but
; swapping ∧ for ∨ and vice versa.
(define (cnf f)
  (unnest (distribute '∨ '∧ (nnf f))))

; Returns a formula that is equivalent to f but
; uses only the connectives allowed by NNF: ∧∨¬.
(define (nnf-ops f)
  (match f
    [`(→ ,(app nnf-ops f1) ,(app nnf-ops f2))
     `(∨ (¬ ,f1) ,f2)]
    [`(↔ ,(app nnf-ops f1) ,(app nnf-ops f2))
     `(∧ (∨ (¬ ,f1) ,f2) (∨ (¬ ,f2) ,f1))]
    [`(,op ,fs ...)
     `(,op ,@(map nnf-ops fs))]
    [_ f]))

; Assuming that f contains only the NNF connectives,
; returns a formula that is equivalent to f but with
; negations (¬) occuring only in literals.
(define (nnf-neg f)
  (match f
    [`(¬ ⊥) `⊤]
    [`(¬ ⊤) `⊥]
    [`(¬ ,(? symbol?)) f]
    [`(¬ (¬ ,f1)) (nnf-neg f1)]
    [`(¬ (∧ ,fs ...))
     `(∨ ,@(for/list ([fi fs]) (nnf-neg `(¬ ,fi))))]
    [`(¬ (∨ ,fs ...))
     `(∧ ,@(for/list ([fi fs]) (nnf-neg `(¬ ,fi))))]
    [`(,op ,fs ...)
     `(,op ,@(map nnf-neg fs))]
    [_ f]))

; Assuming that f is in NNF, distributes ⊗ over ⊕,
; where ⊗, ⊕ ∈ { ∧, ∨ }.
(define (distribute ⊗ ⊕ f)
  (match f
    [`(,(== ⊗) ,gs ... (,(== ⊕) ,fs ...) ,hs ...)
     (distribute ⊗ ⊕
      `(,⊕ ,@(for/list ([fi fs]) `(,⊗ ,fi ,@gs ,@hs))))]
    [`(,(== ⊕) ,fs ...)
     `(,⊕ ,@(for/list ([fi fs]) (distribute ⊗ ⊕ fi)))]
    [_ f]))

; Assuming that f is in NNF, transforms it to flatten
; all nested subformulas of the form (∧ ... (∧ ...) ...)
; and (∨ ... (∨ ...) ...).
(define (unnest f)
  (match f
    [`(∨ ,gs ... (∨ ,fs ...) ,hs ...)
     (unnest `(∨ ,@gs ,@fs ,@hs))]
    [`(∧ ,gs ... (∧ ,fs ...) ,hs ...)
     (unnest `(∧ ,@gs ,@fs ,@hs))]
    [`(,op ,fs ...)
     `(,op ,@(map unnest fs))]
    [_ f]))