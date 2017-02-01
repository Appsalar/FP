(define (zad1 f l)
  (define (helper res lst)
    (if (null? lst)
        res
        (if (member (f (car lst)) l)
            (helper (cons (car lst) res) (cdr lst))
            (helper res (cdr lst)))))
  (reverse (helper '() l)))

(define T '(16
            (8 (6 () ()) (9 () ()))
            (20 () ())))

(define root car)
(define empty-tree? null?)
(define left-tree cadr)
(define right-tree caddr)
(define make-tree list)

(define (insert t x)
  (if (empty-tree? t)
      (list x '() '())
      (if (< x (root t))
          (make-tree
           (root t)
           (insert (left-tree t) x)
           (right-tree t))
          (make-tree
           (root t)
           (left-tree t)
           (insert (right-tree t) x)))))

(define G '((1 2)
            (2)
            (3 2)
            (4)))

(define (vertices g)
  (map car g))

(define (cnt x l)
  (if (null? l)
      0
      (if (= x (car l))
          (+ 1 (cnt x (cdr l)))
          (cnt x (cdr l)))))

(define (io x g)
  (- (cnt x (apply append g)) 1))

(define (odd-pow g)
  (map car (filter (lambda (x) (odd? (+
                             (io (car x) g)
                             (- (length x) 1)))) g)))
  ;(map (lambda(x) (any (lambda(y) (member (car x) 
   ;        (vertices g))