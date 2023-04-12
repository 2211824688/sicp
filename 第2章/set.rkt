#lang racket

; 集合作为未排序的表

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))

;集合作为排序的表

(define (element-of-order-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-order-set? x (car set)))))

(define (adjoin-order-set a set)
  (cond
    ((null? set) (cons a set))
    ((= (car set) a) set)
    ((> (car set) a)
      (cons a set))
    (else 
      (cons (car set)
            (adjoin-order-set a (cdr set))))))

(define (intersection-order-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-order-set (cdr set1)
                                             (cdr set2))))
              ((< x1 x2)
               (intersection-order-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-order-set set1 (cdr set2)))))))

(define (union-order-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                       (union-order-set (cdr set1)
                                        (cdr set2))))
                 ((< x1 x2)
                  (cons x1
                        (union-order-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons x2
                        (union-order-set set1 (cdr set2)))))))))

; 集合作为二叉树

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))

; 二叉树 to 有序表
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; 有序表 to 平衡二叉树

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


; test
(define set1 '(1 2 3))
(define set2 '(4 5 6))

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


(tree->list-1 t1)
(tree->list-2 t1)

(define list '(1 3 5 7 9 11))

(list->tree list)





