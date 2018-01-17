;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname encryption) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (euclid-gcd m n)
  (cond[(= n 0) m]
       [else (euclid-gcd n (remainder m n))]))

(define (prime? m n)
  (cond[(= n m) true]
       [(= (remainder m n) 0) false]
       [else (prime? m (add1 n))]))

(define (convert-string str)
  ;;(generate-checksum (map char-upcase (string->list str)) 0 1))
  (map char->integer (string->list str)))

(define (generate-checksum list1 checksum acc)
  (cond[(empty? list1) checksum]
       [else (generate-checksum (rest list1) (+ checksum (* (- (char->integer (first list1)) 64) acc)) (* acc 100))]))

(define (nearest gen-num diff)
  (cond[(prime? gen-num 2) (list gen-num diff)]
       [else (nearest (+ 1 gen-num) (add1 diff))]))

(define p 73)
(define q 61)
(define en 23)
(define n (* p q))
(define l (* (- p 1) (- q 1)))


(define (encryption str)
  (local[(define listC (convert-string str))
         (define (convert-list list1 acc)
           (cond[(empty? list1) 0]
                [else (+ (* (first list1) acc) (convert-list (rest list1) (* acc 10000)))]))
         (define (encryption1 C)
           (remainder (expt C en) n))
         (define (encrypt list)
           (cond[(empty? list) empty]
                [else (cons (encryption1 (first list)) (encrypt (rest list)))]))]
    ;;(foldr (lambda (x y) (+ (* x 100) y)) 0
    (convert-list (encrypt listC) 1)))
         


      


(define (decryption M)
  (local[(define (decrypt Mind) (cond[(= (- (remainder (expt Mind d) n) 54) -22) 40]
                                     [else (- (remainder (expt Mind d) n) 54)]))
         (define (message->list M)
           (cond[(= (quotient M 10000) 0) (list (remainder M 10000))]
                [else (append (list (remainder M 10000))
                              (message->list (quotient M 10000)))])) 
         (define d (private en 1 0 0 1))
         (define (decrypt/list list1)
           (cond[(empty? list1) empty]
                [else (cons (decrypt (first list1)) (decrypt/list (rest list1)))]))
         (define (convert-list list1 acc)
           (cond[(empty? list1) 0]
                [else (+ (* (first list1) acc) (convert-list (rest list1) (* acc 100)))]))]
    (cypher->string (convert-list (decrypt/list (message->list M)) 1))))
    


(define (private e x2 y2 x1 y1)
  (cond[(zero? (+ (* e x2) (* l y2)))
        (local[(define d (remainder x1 l))]
          (cond[(positive? d) d]
               [else (+ l d)]))]
       [else (local[(define q (quotient (+ (* e x1) (* l y1))
                                        (+ (* e x2) (* l y2))))]
               (private e (- x1 (* q x2)) (- y1 (* q y2)) x2 y2))]))
 


(define (cypher->string C)
  (local[(define (cypher->list C)
           (cond[(= (quotient C 100) 0) (list (remainder C 100))]
                [else (append (list (remainder C 100))
                              (cypher->list (quotient C 100)))]))         
         (define (list->char list)
           (map (lambda (x) (cond[(= x 40) (integer->char 32)]
                                 [else (integer->char (+ x 54))])) list))]     
    (list->string (list->char (cypher->list C)))))
           
