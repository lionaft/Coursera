;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist ls)
  (cond [(null? ls) (aunit)]
        [else (apair (car ls) (racketlist->mupllist (cdr ls)))]))

(define (mupllist->racketlist ls)
  (cond [(aunit? ls) null]
        [else (cons (apair-e1 ls) (mupllist->racketlist (apair-e2 ls)))]))


;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e) (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
                              [e2 (eval-under-env (ifgreater-e2 e) env)])
                         (if (and (int? e1) (int? e2))
                             (if (> (int-num e1) (int-num e2)) (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
                             (error (format "bad MUPL expression: ~v ~v" e1 e2))))]
        [(mlet? e) (let ([var (mlet-var e)]
                         [exp (eval-under-env (mlet-e e) env)])
                      (eval-under-env (mlet-body e) (cons (cons var exp) env)))]
        [(call? e) (let ([fune (eval-under-env (call-funexp e) env)]
                         [envf (eval-under-env (call-actual e) env)])
                    (if (closure? fune)
                       (letrec ([func (closure-fun fune)]
                                [envc (closure-env fune)]
                                [argn (fun-formal func)]
                                [funn (fun-nameopt func)])
                         (if funn
                             (eval-under-env (fun-body func) (append (list (cons argn envf) (cons funn fune)) envc))
                             (eval-under-env (fun-body func) (cons (cons argn envf) envc))))
                       (error (format "bad MUPL expression: ~v" e))))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([eval (eval-under-env (fst-e e) env)])
                    (if (apair? eval)
                        (apair-e1 eval)
                        (error (format "bad MUPL expression: ~v" e))))]
        [(snd? e) (let ([eval (eval-under-env (snd-e e) env)])
                    (if (apair? eval)
                        (apair-e2 eval)
                        (error (format "bad MUPL expression: ~v" e))))]
        [(isaunit? e) (let ([eval (eval-under-env (isaunit-e e) env)])
                        (if (aunit? eval)
                            (int 1)
                            (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [else (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) (ifgreater (var "_x") (var "_y")
                                                         e4
                                                         (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "recur" "list"
            (ifaunit (var "list")
                     (aunit)
                     (apair (call (var "func") (fst (var "list")))
                            (call (var "recur") (snd (var "list"))))))))



(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([help (λ(exp)
                   (cond [(var? exp) (set (var-string exp))]
                         [(add? exp) (set-union (help (add-e1 exp)) (help (add-e2 exp)))]
                         [(ifgreater? exp) (set-union (help (ifgreater-e1 exp)) (help (ifgreater-e2 exp)) (help (ifgreater-e3 exp)) (help (ifgreater-e4 exp)))]
                         [(fun? exp) (set-remove (set-remove (help (fun-body exp)) (fun-formal exp)) (fun-nameopt exp))]
                         [(call? exp) (set-union (help (call-funexp exp)) (help (call-actual exp)))]
                         [(mlet? exp) (set-remove (set-union (help (mlet-e exp)) (help (mlet-body exp))) (mlet-var exp))]
                         [(apair? exp) (set-union (help (apair-e1 exp)) (help (apair-e2 exp)))]
                         [(fst? exp) (help (fst-e exp))]
                         [(snd? exp) (help (snd-e exp))]
                         [(isaunit? exp) (help (isaunit-e exp))]
                         [(closure? exp) (help (closure-fun exp))]
                         [else (set)]))])
    (cond [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e)) (compute-free-vars (ifgreater-e2 e)) (compute-free-vars (ifgreater-e3 e)) (compute-free-vars (ifgreater-e4 e)))]
          [(fun? e) (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (help e))]
          [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
          [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
          [(fst? e) (fst (compute-free-vars (fst-e e)))]
          [(snd? e) (snd (compute-free-vars (snd-e e)))]
          [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
          [(closure? e) (closure (compute-free-vars (closure-env e)) (compute-free-vars (closure-fun e)))]
          [else e])))
                      

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(fun-challenge? e)
         (closure (filter (λ (x) (set-member? (fun-challenge-freevars e) (car x))) env) e)]
        [else (eval-under-env e env)]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))