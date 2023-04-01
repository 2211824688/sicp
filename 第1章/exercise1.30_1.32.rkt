#lang racket

;; 递归定义product
#(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; 迭代定义product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (inc x) (+ x 1))


;; 利用product过程定义阶乘
(define (factorial n)  
  (product identity 1 inc n))

;; 抽象：accumulate的实现

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum-combiner x y) (+ x y))

(define (mul-combiner x y) (* x y))

;; 迭代版本的accumulate
#(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        (combiner result null-value)
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


;; filtered-accumulate
(define (filtered-accumulate is-good? combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (is-good? (term a))
          (combiner (term a) (filtered-accumulate is-good? combiner null-value term (next a) next b))
          (filtered-accumulate is-good? combiner null-value term (next a) next b))))











