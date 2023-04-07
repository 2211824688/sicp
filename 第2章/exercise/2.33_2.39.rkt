#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; 2.33

(define (inc x) (+ x 1))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y)) 0 sequence))

; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 2.35
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda(x) 1) (fringe t))))


; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             null
             (cons (accumulate op init
                               (map (lambda (seq) (car seq)) seqs))
                   (accumulate-n op init
                               (map (lambda (seq) (cdr seq)) seqs)))))

;2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector) cols x)
         m)))

;2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; 2.39

;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) null sequence))


(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

; 嵌套映射

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (small-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  (* x x))
(define (prime? n)
  (= n (small-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

; 排列

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))


; 2.40
(define (enumerate n)
  (if (< n 1)
      null
      (append (enumerate (- n 1))
              (list n))))

(define (unique-pairs n)
  (accumulate append null
              (map
               (lambda (i)
                 (map (lambda (j) (list j i))
                      (enumerate (- i 1))))
               (enumerate n))))

(define (prime-sum-pairs1 n)
  (filter (lambda (p)
            (prime? (+ (car p) (cadr p))))
          (unique-pairs n)))

; 2.41

(define (s-triple n s)
  (filter
    (lambda (triple)
      (let ((a1 (car triple))
            (a2 (cadr triple))
            (a3 (caddr triple)))
      (and (< a3 n)
           (< a2 a3))))
    (map (lambda (p) 
           (append p (list (- s (car p) (cadr p)))))
      (unique-pairs n))))










