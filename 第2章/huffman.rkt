#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; 构造huffman树

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; 解码

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; 构造出树叶的初始排序集合 例如： ((A 4) (B 2) (C 1) (D 1)) -> ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) 
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


; 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree)
;'(A D A B B C A)


; 编码

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "tree cannot be empty!"))
        ((leaf? tree) '())
        (else
         (let ((lb (left-branch tree))
               (rb (right-branch tree)))
           (let ((ls (symbols lb))
                 (rs (symbols rb)))
             (cond ((exist? symbol ls)
                    (cons 0 (encode-symbol symbol lb)))
                   ((exist? symbol rs)
                    (cons 1 (encode-symbol symbol rb)))
                   (else
                    (error "no such character -- " symbol))))))))


(define (exist? symbol symbols)
  (if (null? symbols)
      false
      (or (eq? symbol (car symbols))
          (exist? symbol (cdr symbols)))))

;test
(define message '(A D A B B C A))

;(encode message sample-tree)


; 根据 符号-频度表生成huffuman树

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge
       (adjoin-set (make-code-tree (car pairs)
                                  (cadr pairs))
                   (cddr pairs)))))

;test
(define sym-freq-pairs '((A 4) (B 2) (C 1) (D 1)))
;(generate-huffman-tree sym-freq-pairs)

; 2.70

(define huffman-tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(encode '(GET A JOB) huffman-tree)

(encode '(SHA NA NA NA NA NA NA NA NA) huffman-tree)

(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) huffman-tree)

(encode '(SHA BOOM) huffman-tree)












