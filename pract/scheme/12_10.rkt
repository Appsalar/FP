(define (sum x)
  (if (= x 1)
      1
      (+ x (sum (- x 1)))))

(sum 5)
(sum 30)

(define (fast-pow x n)
(define k (quotient n 2))  
  (cond ((= 1 n) x)
      ((even? n)
          (fast-pow (* x x) (/ n 2)))
      (else
       (* x (fast-pow x (- n 1)))))) ;препраща в горния :D

(fast-pow 2 3)
(fast-pow 2 10)

(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

(define (fact-iter n)
  (define (fact-helper i res)
    (if (= 1 i)
        res
        (fact-helper (- i 1) (* i res))))
  (fact-helper n 1))

(fact-iter 5)


(define (sum-range a b)
  (define (help s res)
    (if (> s b)
       res
       (help (+ s 1) (+ res s))))
  (help a 0))

(sum-range 1 5)

(define (reverse-digits x)
  (define (rev-help tmp res)
    (if (= tmp 0)
        res
        (rev-help (quotient tmp 10)
                  (+ (* res 10) (remainder tmp 10)))))
  (rev-help x 0))

(reverse-digits 1234)

;my shit :D
;(define (fib-iter n)
;  (define (fib-help a b i)
;    (cond ((= n i) (if (even? i) a b))
;          ((even? i) (fib-help (+ a b) b (+ i 1)))
;          (else (fib-help a (+ a b) (+ i 1)))))
;  (fib-help 0 1 0))

(define (fib-iter n)
  (define (fib-help a b i)
    (if (= n i)
        a
          (fib-help b (+ a b) (+ i 1))))
  (fib-help 0 1 0))

(fib-iter 11)
        
(define (is-prime? x)
  (define (prime-help i)
    (if (> i (sqrt x))
        #t
        (if (= 0 (remainder x i))
            #f
            (prime-help (+ i 1))
            )))
  (prime-help 2))

(is-prime? 3)
