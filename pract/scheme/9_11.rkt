(define (list-digits n)
  (define (helper res n)
    (if (= n 0)
        res
        (helper (cons (remainder n 10) res) (quotient n 10))))
  (helper '() n))

(define (list-divisors n)
  (define (helper res k)
    (if (= k 1)
        (cons 1 res) 
            (helper (if (= (remainder n k) 0)
                        (cons k res)
                        res)
                        (- k 1))))
  (helper '() n))

(define (all? p? l)
  (if (null? l)
      #t
      (if (p? (car l))
          (all? p? (cdr l))
          #f)))

(define (any? p? l)
  (not (all? (lambda (x) (not (p? x))) l)))

(define (list-length . lists)
  (map length lists))

(define (1+ x) (+ x 1))
(define (sq x) (* x x))

(define (nmap . fns)
  (lambda (l)
    (define (helper fs res)
      (if (null? fs)
          res
          (helper (cdr fs)
                  (map (car fs) res))))
    (helper fns l)))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2))
            (zip (cdr l1) (cdr l2)))))

(define (insert-sorted x l)
    (if (null? l)
        (list x)
        (if (< x (car l))
            (cons x l)
            (cons (car l) (insert-sorted x (cdr l))))))

(define (insertion-sort l)
  (define (helper res l)
    (if (null? l)
      res
      (helper (insert-sorted (car l) res) (cdr l))))
  (helper '() l))

(define (sum-even-squares l)
  (foldr + 0 (map sq (filter even? l))))

(define (get-scholarships l)
  (map cadr (filter
             (lambda (x) (> (caddr x) 5.5))
               l)))

(define l '( (1234 "ivan" 5.23) (1234 "an" 5.63)
             (1234 "ivan" 5.23) (1234 "iv" 5.83) ))