#lang racket

; 符号化求导程序
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (a1 exp) var)
                   (deriv (a2 exp) var)))
        ((product? exp)
         (make-sum
          (make-product (m1 exp)
                        (deriv (m2 exp) var))
          (make-product (m2 exp)
                        (deriv (m1 exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; exponetiation

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**) (number? (exponent exp))))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

; variable

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; isNumber?

(define (=number? exp num)
  (and (number? exp) (eq? exp num)))

; sum

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (make-sum . args)
  (define (iter result l)
    (if (null? l)
        result
        (let ((first (car l)))
          (if (=number? first 0)
              (iter result (cdr l))
              (iter (append result (list first))
                    (cdr l))))))
  (iter '(+) args))

(define (a1 exp) (cadr exp))

(define (a2 exp)
  (define (iter result l)
    (if (null? l)
        result
        (let ((first (list (car l))))
          (iter (append result first)
                (cdr l)))))
  (if (null? (cdddr exp))
      (caddr exp)
      (iter '(+) (cddr exp))))

; product

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product . args)
  (define (iter res l)
    (if (null? l)
      res
      (iter (append res (list (car l)))
            (cdr l))))
  (iter '(*) args))

(define (m1 exp)
  (cadr exp))

(define (m2 exp)
  (define (iter result l)
    (if (null? l)
      result
      (iter (append result (list (car l)))
            (cdr l))))
  (if (null? (cdddr exp))
    (caddr exp)
    (let ((rest (cddr exp)))
      (iter '(*) rest))))

;test

(define exp (make-sum 1 2 3))

