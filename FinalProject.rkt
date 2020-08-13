;; PL Project - Spring 2020
;; NUMEX interpreter
;;MohammadAli Keshavarz - 9631061

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs --------------------------------------------------------------
(struct var (string) #:transparent)
(struct num (int) #:transparent)
(struct bool (b) #:transparent)
(struct plus (e1 e2) #:transparent)
(struct minus (e1 e2) #:transparent)
(struct mult (e1 e2) #:transparent)
(struct div (e1 e2) #:transparent)
(struct neg (e) #:transparent)
(struct andalso (e1 e2) #:transparent)
(struct orelse (e1 e2) #:transparent)
(struct cnd (e1 e2 e3) #:transparent)
(struct iseq (e1 e2) #:transparent)
(struct ifnzero (e1 e2 e3) #:transparent)
(struct ifleq (e1 e2 e3 e4) #:transparent)
(struct lam (nameopt formal body) #:transparent)
(struct apply (funexp actual) #:transparent)
(struct with (s e1 e2) #:transparent)
(struct apair (e1 e2) #:transparent)
(struct 1st (e) #:transparent)
(struct 2nd (e) #:transparent)
(struct munit () #:transparent)
(struct ismunit (e) #:transparent)
(struct closure (env f) #:transparent)
(struct letrec (s1 e1 s2 e2 s3 e3 e4 #:transparent)
(struct queue (e q) #:transparent)
(struct enqueue (e q) #:transparent)
(struct dequeue (q) #:transparent)
(struct extract (q) #:transparent)


;; Problem 1 ---------------------------------------------------------------------------------------------------

;input is either null, or a racket list, or something invalid
(define (racketlist->numexlist xs)
  (cond[(null? xs) (munit)] ;null in racket is munit in NUMEX
       [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))] ;list in racket is apair in NUMEX(which is applied recursively)
       [else (error ("not a racket list"))]) ;shows that the input is invalid
  )

;input is either null, or a NUMEX list, or something invalid
(define (numexlist->racketlist xs)
  (cond[(munit? xs) null] ;munit in NUMEX is null in racket
       [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))] ;apair in NUMEX is list in racket(which is applied recursively)
       [else (error ("not a NUMEX list"))]) ;shows that the input is invalid
  )

;; Problem 2 ---------------------------------------------------------------------------------------------------

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  		"CHANGE" 
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))