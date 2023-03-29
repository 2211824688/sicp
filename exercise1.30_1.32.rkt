#lang racket
(define (product term a next b)
  (if (> a b)
     1
     (* (term a)
        (product term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ x 1))


;; 利用product过程定义阶乘
(define (factorial n)  
  (product identity 1 inc n))



