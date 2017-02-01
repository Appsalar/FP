(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define root car)

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

(define (sum-tree t)
  (if (empty-tree? t)
      0
      (+ (root t)
         (sum-tree (left-tree t))
         (sum-tree (right-tree t)))))

(define T '(5
            (3
             (1 () ())
             (4 () ()))
            (10
             (7 () ())
             (8 ()
                (9 ()())))))

(define (level t l)
  (if (= l 0)
      (if (empty-tree? t)
          '()
          (list (root t)))
      (append (level (left-tree t) (- l 1))
              (level (right-tree t) (- l 1)))))

(define expr1 '(2 + 3))
(define expr2 '((7 + 4) - (2 * 3)))
(define expr3 '((42 - 6) / 6))

(define (make-leaf x)
  (make-tree x '() '()))

(define (build-expr-tree expr)
  (if (number? expr)
      (make-leaf expr)
      (make-tree (cadr expr)
                 (build-expr-tree (car expr))
                 (build-expr-tree (caddr expr)))))

(define (eval-expr-tree t)
  (if (empty-tree? t)
      0
      (if (number? (root t))
          (root t)
          ((eval (root t)) (eval-expr-tree (left-tree t))
                            (eval-expr-tree (right-tree t))))))

(define G '((1 2 3)
            (2 4)
            (3 5)
            (4 )
            (5 1 6)
            (6 )))

(define (vertices g)
  (map car g))

(define (neightbours g v)
  (cdr (assoc v g)))

(define (edges g)
  (apply append (map (lambda (x)
         (map (lambda (y) (cons (car x) y)) (cdr x))) g)))