(define M '((1 2 3)
            (4 5 6)
            (7 8 9)))

(define (get-rows-count m)
  (length m))

(define (get-cols-count m)
  (if (null? m)
      0
      (length (car m))))

(define get-row list-ref)

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m) (transpose (map cdr m)))))

(define (get-col m i)
  (map (lambda (x) (list-ref x i)) m));(get-row (transpose m) i))

(define (delete-row m i)
  (if (null? m)
      '()
      (if (= i 0)
          (cdr m)
          (cons (car m) (delete-row (cdr m) (- i 1))))))

(define (delete-col m i)
  (transpose (delete-row (transpose m) i)))

(define (get-elem m i j)
  (list-ref (get-row m i) j))

(define (main-diag m)
  (define (helper i)
    (if (= i (get-rows-count m))
        '()
        (cons (get-elem m i i) (helper (+ i 1)))))
  (helper 0))

(define (second-diag m)
  (main-diag (reverse m)))

(define (diagonal-sum m)
  (foldr + 0 (append (main-diag m) (second-diag m))))

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (10
             (7 () ())
             (8 ()
                (9 ()())))))

(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define (root t)
  (if (empty-tree? t)
      '()
      (car t)))

(define left-tree cadr)

(define right-tree caddr)

(define (is-leaf? t)
  (and (empty-tree? (left-tree t))
       (empty-tree? (right-tree t))))

(define (height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (heigth (left-tree t))
                (heigth (right-tree t))))))

(define (contains? t x)
  (if (empty-tree? t)
      #f
      (if (equal? (root t) x)
          #t
          (or (contains? (left-tree t) x)
              (contains? (right-tree t) x)))))

(define (in-order t)
  (if (empty-tree? t)
      '()
      (append (in-order (left-tree t))
              (list (root t))
              (in-order (right-tree t)))))