#lang racket
(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define first
    (lambda (p)
        (car p)))

(define second
    (lambda (p)
        (car (cdr p))))

(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 (quote ())))))

(define shift
    (lambda (pair)
        (build (first (first pair))
            (build (second (first pair))
                (second pair)))))

(define a-pair?
    (lambda (x)
        (cond
            ((atom? x) #f)
            ((null? x) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else #f))))

(define align
    (lambda (pora)
        (cond
            ((atom? pora) pora)
            ((a-pair? (first pora))
                (align (shift pora)))
            (else (build (first pora) (align (second pora)))))))

(define length*
    (lambda (pora)
        (cond
            ((atom? pora) 1)
            (else (o+ (length* (first pora)) (length* (second pora)))))))

(define weight*
    (lambda (pora)
        (cond
            ((atom? pora) 1)
            (else (o+ (x (weight* (first pora)) 2) (weight* (second pora)))))))

(define revpair
    (lambda (pair)
        (build (second pair) (first pair))))

(define shuffle
    (lambda (pora)
        (cond
            ((atom? pora) pora)
            ((a-pair? (first pora)) (shuffle (revpair pora)))
            (else (build (first pora) (shuffle (second pora)))))))

'(first '(a b))
(first '(a b))
'(second '(a b))
(second '(a b))
'(build 'a 'b)
(build 'a 'b)
'(shift '((a b) (c d))) 
(shift '((a b) (c d))) 
'(align '((((a i) h) (b (c f))) ((d g) e)))
(align '((((a i) h) (b (c f))) ((d g) e)))
'(length* '((a b) c))
(length* '((a b) c))
'(length* '(a (b c)))
(length* '(a (b c)))
'weight*
(weight* '((a b) c))
(weight* '(a (b c)))
'revpair
(revpair '(a b))
'(shuffle ((a b) (c d))) 
'(shuffle '((a b) (c d))) 
