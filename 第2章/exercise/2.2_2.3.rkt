#lang racket

;2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (midpoint-segment seg)
  (let ((x1 (x-point (start-segment seg)))
        (y1 (y-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y2 (y-point (end-segment seg))))
    (make-point (/ (+ x1 x2) 2)
                (/ (+ y1 y2) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;2.3
(define (square x) (* x x))

(define (length s)
  (let ((dx (- (x-point (end-segment s)) (x-point (start-segment s))))
        (dy (- (y-point (end-segment s)) (y-point (start-segment s)))))
    (sqrt (square dx) (square dy))))

(define (make-rect seg1 seg2)
  (cons (length seg1) (length seg2)))

(define (width-rect rect)
  (car rect))

(define (length-rect rect)
  (cdr rect))

(define (calc-perimeter rect)
  (+ (* 2 (length-rect rect))
     (* 2 (width-rect rect))))

(define (calc-area rect)
  (* (length-rect rect)
     (width-rect rect)))




