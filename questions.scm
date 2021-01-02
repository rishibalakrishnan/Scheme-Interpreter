(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests))

(define (zip pairs)
  (if (null? pairs)
      '(() ())
      (if (null? (car pairs))
          nil
          (cons (first-of pairs) (zip (map cdr pairs))))))

(define (first-of pairs)
  (if (null? pairs)
      nil
      (cons (car (car pairs)) (first-of (cdr pairs)))))

; ; Problem 16
; ; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper n s)
    (if (null? s)
        nil
        (cons (list n (car s)) (helper (+ n 1) (cdr s)))))
  (helper 0 s))

; END PROBLEM 16
; ; Problem 17
; ; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (cond 
    ((null? denoms)
     nil)
    ((< total 0)
     nil)
    ((= total 0)
     '(()))
    (else
     (append (cons-all (car denoms)
                       (list-change (- total (car denoms)) denoms))
             (list-change total (cdr denoms))))))

; END PROBLEM 17
; ; Problem 18
; ; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))

(define define? (check-special 'define))

(define quoted? (check-special 'quote))

(define let? (check-special 'let))

; ; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((atom? expr)
     expr)
    ((quoted? expr)
     expr)
    ((or (lambda? expr) (define? expr))
     (let ((form (car expr))
           (params (cadr expr))
           (body (cddr expr)))
       (cons form (cons params (map let-to-lambda body)))))
    ((let? expr)
     (let ((values (cadr expr))
           (body (cddr expr))
           (inside (zip (let-to-lambda (cadr expr)))))
       (cons (cons 'lambda
                   (cons (car inside) (let-to-lambda body)))
             (cadr inside))))
    (else
     ; BEGIN PROBLEM 18
     (map let-to-lambda expr)
     ; END PROBLEM 18
    )))
