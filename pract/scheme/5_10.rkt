

(define (** x)
  (* x 2))

(define (is-even? x)
  (= (remainder x 2) 0))

(define (is-odd? x)
  (not (is-even? x)))

(define (my-abs? x)
  (if(< x 0) (- x) x))

(my-abs? -5)

(define (fact n)
  (if(= n 0) 1 (* n (fact (- n 1)))))
(define (fib n)
 (if(< n 2)
    n
     (+ (fib(- n 2)) (fib(- n 1)))))

;(fib 1)
;(fib 2)
;(fib 3)
;(fib 4)
;(fib 5)


(define (count-digits n)
  (if(= (quotient n 10) 0)
     1
     (+ 1 (count-digits (quotient n 10)))))

(count-digits 1)
(count-digits 12)
(count-digits 123)


(define (sum-digits n)
  (if(< n 10)
     n
     (+ (remainder n 10) (sum-digits (quotient n 10)))))

(quote "laina laina")
(sum-digits 0)
(sum-digits 1)
(sum-digits 123)
(sum-digits 45)
(sum-digits 12000)

(quote "laina laina")

(define (pow a b)
  (if(= b 0)
     1
     (* a (pow a (- b 1)))))

(pow 2 3)
(pow 4 2)

(quote "laina laina")

(define (reverse-digits n)
(if(< n 10)
   n
   (+ (* (pow 10 (- (count-digits n) 1)) (remainder n 10))
      (reverse-digits (quotient n 10)))))

(reverse-digits 123)
(reverse-digits 1234)
