#lang racket

;2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;test
(define z (make-from-mag-ang 1 3.14))

(real-part z)
(imag-part z)
(magnitude z)
(angle z)

