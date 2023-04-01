#lang racket

(define (square x) (* x x))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
              (cond ((positive? test-value)
                     (search f neg-point midpoint))
                    ((negative? test-value)
                     (search f midpoint pos-point))
                     (else midpoint))))))

(define (average a b) (/ (+ a b) 2))


(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (cube x) (* x x x))
;; 二分寻找函数零点
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

;; 寻找函数的不动点
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (dis x)
    (newline)
    (display x)
    (try x))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
           next
          (dis next))))
  (try first-guess)
  (newline))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (cont-frac n d k)
  (define (iter index)
    (if (> index k)
        0
        (/ (n index) (+ (d index) (iter (+ index 1))))))
  (iter 1))
      

; 计算黄金分割
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           100)

(define (d i)
  (cond
    ((or (= i 1) (= i 2)) i)
    ((= 0 (remainder (- i 2) 3))
      (* 2 (+ 1 (/ (- i 2) 3))))
    (else 1)))

; 计算e
;(+ 2 (cont-frac (lambda (i) 1.0) d 100))


(define (cont-frac2 n d k)
  (define (iter index)
    (if (> index k)
        0
        (/ (n index) (- (d index) (iter (+ index 1))))))
  (iter 1))

; 计算正切函数
(define (tan-cf x k)
  (define (n i)
    (if (= i 0)
        1
        (* x (n (- i 1)))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac2 n d k))

; 平均阻尼

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

; 计算立方根
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; 导数

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  fixed-point (transform g) guess)


; exercise 1.40_1.46

(define (cubic a b c)
  (lambda (x) 
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (inc n) (+ n 1))
(define (double f)
  (lambda (x)
    (f (f x))))







