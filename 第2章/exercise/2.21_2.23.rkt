#lang racket

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; 2.21
(define (square x) (* x x))

(define (square-list1 items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (square x))
       items))

(define l (list 1 2 3 4 5))

; 2.22 
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

; 2.23
(define (for-each1 proc items)
  (define (iter proc items)
    (if (null? items)
        1024
        (begin (proc (car items))
               (iter proc (cdr items)))))
  (iter proc items))

(define (for-each2 proc items)
  (if (null? items)
      #t
      (begin (proc (car items))
             (for-each2 proc (cdr items)))))

(for-each1 (lambda (x) (display x) (newline))
          (list 57 321 88))

(for-each2 (lambda (x) (display x) (newline)) l)






