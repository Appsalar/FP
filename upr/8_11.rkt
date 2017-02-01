(define (len lst)
  ;(if (null? lst)
   ;   0
    ;  (len (cdr lst))))
  (define (helper res lst)
    (if ( null? lst)
        res
        (helper (+ 1 res) (cdr lst))))
    (helper 0 lst))

(define (reverse* lst)
  (define (helper res lst)
    (if (null? lst)
        res
        (helper (cons (car lst) res) (cdr lst))))
  (helper '() lst))

(define (nth n lst)
  (if (null? lst)
      #f
      (if (= n 0)
          (car lst)
          (nth (- n 1) (cdr lst)))))

(define (range from to)
  (if (> from to)
      '()
      (cons from (range (+ from 1) to))))

(define (take n lst)
  (if (null? lst)
      '()
      (if (= n 0)
          '()
          (cons (car lst) (take (- n 1) (cdr lst))))))

(define (drop n lst)
  (if (null? lst)
      '()
      (if (= n 0)
          lst
          (drop (- n 1) (cdr lst)))))

(define (chunk n lst)
  (if (null? lst)
      '()
      (cons (take n lst) (chunk n (drop n lst)))))

(define (all p? lst)
  (if (null? lst)
      #t
      (if (p? (car lst))
          (all p? (cdr lst))
          #f)))

(define (any p? lst)
  (if (null? lst)
      #t
      (if (p? (car lst))
          #t
          (any p? (cdr lst)))))

(define (any* p? lst)
  (not (all (lambda (x) (not (p? x))) lst)))


(define filter
    (lambda (pred lst)
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst)
                                    (filter pred (cdr lst))))
            (else (filter pred (cdr lst))))))

(define (foldr  op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr op nv (cdr lst)))))

(define (group-pairs lst)
  (define firsts (filter
               (lambda (x)
                 (equal? (cdar lst) (cdr x))) lst))
  (define rests (filter
               (lambda (x)
                 (not (equal? (cdar lst) (cdr x)))) lst))
  (if (null? lst)
      '()
      (cons firsts (group-pairs rests))))

(define (combine-groups lst)
  (map (lambda (l) (list (cdar l) (map car l))) lst))

(define (group-by-f f lst)
  (combine-groups (group-pairs
                   (map (lambda (x) (cons x (f x))) lst))))
