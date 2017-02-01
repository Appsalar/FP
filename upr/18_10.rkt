(define (log10 n)
  (/ (log n ) (log 10)))

(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (log10 n))))

(define (reverse-digits n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10)
                (+ (* res 10) (remainder n 10)))))
  (helper n 0))

(define (divisors-sum n)
  (define (help i sum)
    (if (> i n)
        sum
        (help (+ i 1)
              (+ sum (if (= 0 (remainder n i))
                          i
                          0)))))
  (help 1 0))

(define (perfect? n)
  (= (* 2 n) (divisors-sum n)))

(define (prime? n)
  (define (helper i)
    (if (> i (sqrt n))
        #t
        (if (= (remainder n i) 0)
            #f
            (helper (+ 1 i)))))
  (if (= n 1)
      #f
      (helper 2)))

(define (toBinary n)
  (if (= n 0)
      0
      (+ (* 10 (toBinary (quotient n 2)))
         (remainder n 2))))

(define (toBin n)
  (define (helper n res pos)
    (if (= n 0)
        res
        (helper (quotient n 2)
                (+ res (* (remainder n 2)
                         (expt 10 pos)))
                (+ pos 1))))
  (helper n 0 0))