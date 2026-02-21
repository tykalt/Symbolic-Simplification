#lang racket

; Helper function to apply correct operators
(define (apply-operator op a b)
  (match op
    ['+ (+ a b)]
    ['- (- a b)]
    ['* (* a b)]
    ['/ (/ a b)]
  )
)

; Phase 1 - Constant Folding
(define (fold-constants expr)
  (match expr
    [(? number?) expr]
    [(? symbol?) expr]
    [(list op lhs rhs)
     (define lhs2 (fold-constants lhs))
     (define rhs2 (fold-constants rhs))
     (if (and (number? lhs2) (number? rhs2))
         (apply-operator op lhs2 rhs2)
         (list op lhs2 rhs2))]))


; Helper function to canonicalize commutative op expressions
(define (canonicalize op a b)
  (cond
    [(and (eq? op '+) (number? a) (symbol? b)) (list '+ b a)]
    [(and (eq? op '*) (number? a) (symbol? b)) (list '* b a)]
    [else (list op a b)]))


; Helper function to extract var-coeff term
(define (like-term-extract expr)
  (match expr
    [(list '* (? symbol? x) (? number? a)) (list x a)]
    [_ #f]))


; Phase 2 - Algebraic Identities
(define (reduce-identities expr)
  (match expr
    [(? number?) expr]
    [(? symbol?) expr]

    ; Double negatives
    [(list '- (list '- x))
     (reduce-identities x)]

    [(list op lhs rhs)
     (define lhs2 (reduce-identities lhs))
     (define rhs2 (reduce-identities rhs))

     ; Canonicalize
     (define expr2 (canonicalize op lhs2 rhs2))
     (match expr2

       ; Addition property
       [(list '+ x 0) x]
       [(list '+ 0 x) x]

       ; Like-term case handle
       [(list '+ a b)
        (define t1 (like-term-extract a))
        (define t2 (like-term-extract b))
        ; if share common variable
        (if (and t1 t2 (eq? (first t1) (first t2)))
            ; fold-constants function will handle the addition later
            (list '* (first t1) (list '+ (second t1) (second t2)))
            expr2)]

       ; Zero property
       [(list '* _ 0) 0]
       [(list '* 0 _) 0]

       ; Multiplication property
       [(list '* x 1) x]
       [(list '* 1 x) x]

       ; Final fallback
       [_ expr2])]))
       

; Phase 3 - Fixed-Point Iteration
(define (simplify-once expr)
  (reduce-identities (fold-constants expr)))

; Keep applying simplify-once until the expression stops changing
(define (simplify expr)
  (define next (simplify-once expr))
  (if (equal? next expr)
      expr
      (simplify next)))

                 

            
            
  

          
       





