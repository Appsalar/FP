(define root car)
(define left cadr)
(define right caddr)
(define empty-tree? null?)

(define (treeSum t)
  (if (empty-tree? t)
      0
      (+ (root t)
         (treeSum (left t))
         (treeSum (right t)))))

(define (transformSum t)
  (if (empty-tree? t)
      '()
      (list (treeSum t)
            (transformSum (left t))
            (transformSum (right t)))))

(define T '(3
            (2 () ())
            (4
             (1 ()())
             (5 ()
                (2 ()())))))

(define G '((a 2 b c)
            (b 4 a c)
            (c 1 a b)))

(define (vertices g)
  (map car g))

(define (neightbours g v)
  (cddr (assoc v g)))

(define (edges g)
  (apply append (map (lambda (x)
         (map (lambda (y) (cons (car x) y)) (cddr x))) g)))

;(define (euler g)
 ; (define (helper cur rest)
   ; (