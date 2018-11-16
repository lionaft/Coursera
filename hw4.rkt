
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([seq-t (lambda (l acc) 
    (if (> l high) (sort acc <) (seq-t (+ l stride) (cons l acc))))])
    (seq-t low null)))

(define (string-append-map xs suffix)
  (map (λ (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0)  (error "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
    (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
         (letrec ([f (λ (x) (if (= (remainder x 5) 0) (cons (- 0 x) (λ () (f (+ x 1)))) (cons x (λ () (f (+ x 1))))))])
           (λ () (f 1))))

(define dan-then-dog
  (letrec ([dan (λ () (cons "dan.jpg" (λ () (dog))))]
           [dog (λ () (cons "dog.jpg" (λ () (dan))))])
    (λ () (dan))))

(define (stream-add-zero s)
  (letrec ([f (λ (s)
                (let ([str (s)])
                  (cons (cons 0 (car str)) (λ () (f (cdr str))))))])
    (λ () (f s))))

(define (cycle-lists xs ys)
      (letrec ([f 
                (λ (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (λ () (f (+ n 1)))))])
               (λ () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (λ (n)
                (cond  [(= len n) false]
                       [(pair? (vector-ref vec n)) (if (equal? v (car (vector-ref vec n))) (vector-ref vec n) (f (+ n 1)))]
                       [else (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [x 0]
           [y (- n 1)]
           [f (λ (v)
                (let ([res (vector-assoc v memo)])
                  (if res                     
                      res
                      (let ([r (assoc v xs)])
                        (begin (vector-set! memo x r)
                               (set! x (if (= x y) 0 (+ x 1)))
                               r)))))])
    f))
                            
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e e1]
              [f (λ ()
                   (if (< e2 e)
                       (f)
                       #t))])
       (f))]))
            
    
    
  