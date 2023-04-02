#lang racket

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (or (and (> n 0) (> d 0))
              (and (< n 0) (< d 0)))
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (- (/ (abs n) g)) (/ (abs d) g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

