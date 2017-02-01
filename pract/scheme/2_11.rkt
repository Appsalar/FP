(define p
  (cons 3 (cons 4  5)))

(pair? p)
(pair? 5)
(car p)
(cdr p)
(car (cdr p))
(cadr p)

(define empty-list '())
(list? '())
(null? '())
(cons 4 '())

(define l1 '(1 2 3 4 5))
(define l3 (cddddr l1))
(car l3)
(cdr l3)

(define rekt (cons l1 l3))

(define (len lst)
  (if(null? lst)
     0
     (+ 1 (len (cdr lst)))))

(define l2 '(1 3.14 "hi" #f (3.7)))

; нормално = не става за сложни обекти
(define (member? x lst)
  (if (null? (cdr lst))
      #f
      (if (equal?  x (car lst))
          #t
          (member? x (cdr lst)))))

(define (elem-at i lst)
  (if (= i 0)
      (car lst)
      (elem-at (- i 1) (cdr lst))))

(define (rev1 lst)
  (if (null? lst)
      '()
      (cons (reverse (cdr lst)) (car lst))))

(define (rev lst)
  (define (rev-helper res list)
    (if (null? list)
        res
        (rev-helper (cons (car list) res) (cdr list))))
  (rev-helper '() lst))

(define (push-front x lst)
  (cons x lst))

(define (push-back x lst)
  (rev (push-front x (rev lst))))

(define (insert x lst)
  (if (null? lst)
      (cons x '())
      (cons (car lst) (insert x (cdr lst)))))

(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))))

(my-map (lambda (x) (+ x 2)) '(1 2 3))

(define (filter f lst)
  (if (null? lst)
      '()
      (if (f (car lst))
          (cons (car lst) (filter f (cdr lst)))
          (filter f (cdr lst)))))

(filter even? '(1 2 3))

(define (fold st f lst)
  (if (null? lst)
      st
      (f (car lst) (fold st f (cdr lst)))))

(fold 0 + '(1 2 3))