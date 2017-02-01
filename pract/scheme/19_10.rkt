(define (prod init a b)
  (define (helper i res)
    (if (> i b)
        res
        (helper (+ i 1) (* res i))))
  (helper a init))

(prod 2 3 5)

(define (palindrome? x)
  (define (reverse cur res)
    (if (= cur 0)
        res
        (reverse (quotient cur 10)
                 (+ (remainder cur 10) (* res 10)))))
  (= x (reverse x 0)))

(palindrome? 121)
(palindrome? 1234)

(define (perfect? x)
  (define (div-sum i res)
    (if (= x i)
        res
        (div-sum (+ i 1)
                 (if (= 0 (remainder x i))
                     (+ res i)
                     res))))
  (= x (div-sum 1 0)))


(define (circle-area r)
  (let ((pi 3.141592))
    (* pi r r)))

(define (circle-per r)
  (let ((pi 3.141592))
    (* 2 pi r)))

(define (s a b c)
  (let ((p (/ (+ a b c) 2)))
    (sqrt(* p (- p a) (- p b) (- p c)))))

(define (s2 a b c)
  (let* ((P (+ a b c))
    (p (/ P 2)))
     (sqrt(* p (- p a) (- p b) (- p c)))))

(s2 3 4 5)

(define (solve a b c)
  (let* ((2a (* 2 a)) (4ac (* 4 a c))
                      (d (- (* b b) 4ac))
                      (x1 (/ (- (- b) (sqrt d)) 2a))
                      (x2 (/ (+ (- b) (sqrt d)) 2a)))
    (cond ((= a 0) (/ (- b) c))
          ((>= d 0) (list x1 x2))
          (else (write "nrk")))))

(define (circle-area2 r)
  (* ((lambda () 3.14159)) r r))

(circle-area2 5)

(define pi
  (lambda () 3.1415))

(define (apply-twice f x)
  (f (f x)))

(apply-twice (lambda(x) (+ x 2)) 2)

(define (sum-cubes a b)
  (define (cube x ) (expt x 3))
  (define (sum f a b)
    (define (help i res)
      (if (> i b)
          res
          (help (+ i 1) (+ res (f i)))))
    (help a 0))
  (sum cube a b))

(sum-cubes 1 3)

(define (cube x ) (expt x 3))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose abs cube) -3)