#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; 2.46
(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (car v))
             (* s (cdr v))))

; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame1 frame)
  (car frame))
(define (edge1-frame1 frame)
  (car (cdr frame)))
(define (edge2-frame1 frame)
  (cdr (cdr frame)))

;test
(define ori (make-vect 1 1))
(define e1 (make-vect 2 3))
(define e2 (make-vect 3 5))
(define frame (make-frame1 ori e1 e2))

(origin-frame1 frame)
(edge1-frame1 frame)
(edge2-frame1 frame)


  
; 2.48

(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

; 2.49

; (a)

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 0))
(define v3 (make-vect 1 1))
(define v4 (make-vect 0 1))

(define edge-painter (segments->painter
                      (list (make-segment v1 v2)
                            (make-segment v2 v3)
                            (make-segment v3 v4)
                            (make-segment v4 v1))))

; (b)

(define fork-painter (segments->painter (list (make-segment v1 v3)
                                             (make-segment v2 v4))))


; (c)
(define v5 (make-vect 0.5 0))
(define v6 (make-vect 1 0.5))
(define v7 (make-vect 0.5 1))
(define v8 (make-vect 0 0.5))

(define diamond-painter (segments->painter (list (make-segment v5 v6)
                                                (make-segment v6 v7)
                                                (make-segment v7 v8)
                                                (make-segment v8 v5))))

; (d)

(define wave
  (segments->painter
    (list
      (make-segment (make-vect 0 0.65) (make-vect 0.14 0.39))
      (make-segment (make-vect 0.14 0.39) (make-vect 0.29 0.58))
      (make-segment (make-vect 0.29 0.58) (make-vect 0.34 0.49))
      (make-segment (make-vect 0.34 0.49) (make-vect 0.24 0))
      (make-segment (make-vect 0.4 0) (make-vect 0.49 0.28))
      (make-segment (make-vect 0.49 0.28) (make-vect 0.59 0))
      (make-segment (make-vect 0.71 0)  (make-vect 0.59 0.45))
      (make-segment (make-vect 0.59 0.45) (make-vect 0.99 0.15))
      (make-segment (make-vect 0.99 0.35) (make-vect 0.74 0.64))
      (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
      (make-segment (make-vect 0.74 0.64) (make-vect 0.59 0.64))
      (make-segment (make-vect 0.59 0.64) (make-vect 0.64 0.85))
      (make-segment (make-vect 0.64 0.85) (make-vect 0.59 1))
      (make-segment (make-vect 0.39 1) (make-vect 0.34 0.85))
      (make-segment (make-vect 0.34 0.85) (make-vect 0.39 0.64))
      (make-segment (make-vect 0.39 0.64) (make-vect 0.29 0.64))
      (make-segment (make-vect 0.29 0.64) (make-vect 0.14 0.6))
      (make-segment  (make-vect 0.14 0.6) (make-vect 0 0.84)))))


; 2.50

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter 
          (make-frame new-origin
            (sub-vect (m corner1) new-origin)
            (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))

; 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 0 1.0)
                              (make-vect 1.0 0.5)))
          (paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 1.0 0.0))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))


; 2.52




