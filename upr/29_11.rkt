(define (member? x lst)
  (if (member x lst) #t #f))

(define G '((a b c d)
            (b e f)
            (c a d)
            (d b c g)
            (e)
            (f b e)
            (g a)))

(define (vertices g)
  (map car g))

(define (successors v g)
  (let ((result (assoc v g)))
    (if result
        (cdr result)
        result)))

(define (has-edge? u v g)
  (member? v (successors u g)))

(define (add-vertex v g)
  (if (member v (vertices g))
      g
      (cons (list v) g)))

(define (add-edge u v g)
  (if (has-edge? u v g)
      g
      (let ((newg (add-vertex u (add-vertex v g))))
        (map (lambda (l) (if (equal? (car l) u)
                             (append l (list v))
                             l)) newg))))

(define (make-from-edges lst)
  (foldr (lambda (e g) (add-edge (car e)
                                 (cdr e)
                                 g))
         '() lst))

(define (contains-path? path g)
  (cond ((null? path) #t)
        ((null? (cdr path)) (member? path g))
        ((has-edge? (car path) (cadr path) g)
         (contains-path? (cdr path) g))
        (else #f)))
