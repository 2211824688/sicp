#lang racket

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; 2.25

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(car (cdr (car (cdr (cdr l1)))))
;(car (car l2))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

; 2.26

(define x (list 1 2 3))

(define y (list 4 5 6))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

;(append x y)
;(cons x y)
;(list x y)

; 2.27
(define (deep-reverse l)
  (cond ((null? l) null)
        ((pair? (car l))
         (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
        (else
         (append (deep-reverse (cdr l)) (list (car l))))))

; 2.28
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x1 (list (list 1 2) (list 3 4)))

; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; (a)
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)

; (b)
(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (let ((l-branch (left-branch mobile))
        (r-branch (right-branch mobile))
        (l-structure (branch-structure (left-branch mobile)))
        (r-structure (branch-structure (right-branch mobile))))
    (+ (if (pair? l-structure)
           (total-weight l-structure)
           l-structure)
       (if (pair? r-structure)
           (total-weight r-structure)
            r-structure)))))

(define lBranch (make-branch 5 10))
(define rBranch (make-branch 10 5))

(define m1 (make-mobile lBranch rBranch))

(define m2 (make-mobile (make-branch 4 m1) (make-branch 5 m1)))

(define m3 (make-mobile (make-branch 20 m2) (make-branch 15 m1)))

; (c)
(define (balance? mobile)
    (if (not (pair? mobile))
        #t
        (let ((left-structure (branch-structure (left-branch mobile)))
              (right-structure (branch-structure (right-branch mobile)))
              (left-length (branch-length (left-branch mobile)))
              (right-length (branch-length (right-branch mobile))))
          (and (= (* (total-weight left-structure) left-length)
                  (* (total-weight right-structure) right-length))
               (balance? left-structure)
               (balance? right-structure)))))

; (d)

(define (make-mobile1 left right)
  (cons left right))

(define (make-branch1 length structure)
  (cons length structure))

(define left-branch1 car)
(define right-branch1 cdr)
(define branch-length1 car)
(define branch-structure1 cdr)


; 2.30

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree1 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))
         
(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


; 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (square x) (* x x))
(define (square-tree2 tree)
  (tree-map square tree))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (r) (append (list (car s)) r))
                      rest)))))

(define set (list 1 2 3))

; test
(subsets set)













