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
(struct letrec (s1 e1 s2 e2 s3 e3 e4) #:transparent)
(struct queue (e q) #:transparent)
(struct enqueue (e q) #:transparent)
(struct dequeue (q) #:transparent)
(struct extract (q) #:transparent)


;; Problem 1 ---------------------------------------------------------------------------------------------------

(define (racketlist->numexlist xs)
  (cond[(null? xs) (munit)] 
       [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))] 
       [else (error ("not a racket list"))])
  )


(define (numexlist->racketlist xs)
  (cond[(munit? xs) null] 
       [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))] 
       [else (error ("not a NUMEX list"))])
  )

;; Problem 2 ---------------------------------------------------------------------------------------------------

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (string? str) #f) (error ("str is not a string"))]
        [(equal?(list? env) #f) (error ("env in not a racket list"))]
        [(equal? str (car (car env))) (cdr (car env))] 
        [else (envlookup (cdr env) str)]
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond[(var? e) 
         (cond[(string? (var-string e))(envlookup env (var-string e))]
              [else (error "NUMEX var applied to non-racket-string")])]

        [(num? e)
          (if (integer? (num-int e))
            e (error "num applied to non-integer"))]
        
        [(bool? e)
         (if (boolean? (bool-b e))
            e (error "bool applied to non-boolean"))]

        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
               (cond[(and (num? v1)(num? v2)) (num (- (num-int v1) (num-int v2)))]
                    [else (error "NUMEX subtraction applied to non-number")]))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        

        [(div? e)
          (let ([v1 (eval-under-env (div-e1 e) env)]
                [v2 (eval-under-env (div-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
                (num (quotient (num-int v1)
                        (num-int v2)))
                (error "NUMEX division applied to non-num")))]

        [(neg? e)
          (let ([v (eval-under-env (neg-e e) env)])
            (cond [(num? v)
                    (num (- (num-int v)))]
                  [(bool? v)
                    (if (eq? (bool-b v) #f)
                      (bool #t)
                      (bool #f))]
                  [#t (error "NUMEX negation applied to non-num and non-bool")]))]
        
         [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (cond[(bool? v1)(cond[(equal? (bool-b v1) #f) v1]
                                [else (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                      (cond[(bool? v2) v2]
                                           [else (error "NUMEX conjunction applied to non-number")]))])]
                [else (let ([v2 (eval-under-env (andalso-e2 e) env)])
                      (cond[(bool? v2) v2]
                           [else (error "NUMEX conjunction applied to non-number")]))]))]

         [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (cond[(bool? v1)(cond[(equal? (bool-b v1) #t) v1]
                                [else (let ([v2 (eval-under-env (orelse-e2 e) env)])
                                      (cond[(bool? v2) v2]
                                           [else (error "NUMEX conjunction applied to non-number")]))])]
                [else (let ([v2 (eval-under-env (orelse-e2 e) env)])
                      (cond[(bool? v2) v2]
                           [else (error "NUMEX conjunction applied to non-number")]))]))]
       
       

        [(cnd? e)
          (let ([v (eval-under-env (cnd-e1 e) env)])
            (if (bool? v)
              (if (eq? (bool-b v) #t)
                (eval-under-env (cnd-e2 e) env)
                (eval-under-env (cnd-e3 e) env))
              (error "NUMEX condition applied to non-bool")))]
        
        [(iseq? e)
          (let ([v1 (eval-under-env (iseq-e1 e) env)]
                [v2 (eval-under-env (iseq-e2 e) env)])
            (cond [(and (num? v1)
                        (num? v2))
                    (bool (eq? (num-int v1)
                               (num-int v2)))]
                  [(and (bool? v1)
                        (bool? v2))
                    (bool (eq? (bool-b v1)
                               (bool-b v2)))]
                  [(or (and (num? v1)
                            (bool? v2))
                       (and (bool? v1)
                            (num? v2)))
                    (bool #f)]
                  [#t (error "NUMEX equality applied to non-num and non-bool")]))]
        
         [(ifnzero? e)
          (let ([v (eval-under-env (ifnzero-e1 e) env)])
            (if (num? v)
              (if (eq? (num-int v) 0)
                (eval-under-env (ifnzero-e3 e) env)
                (eval-under-env (ifnzero-e2 e) env))
              (error "NUMEX condition applied to non-number")))]
         
         [(ifleq? e)
          (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                [v2 (eval-under-env (ifleq-e2 e) env)])
            (if (and (num? v1) (num? v2))
              (if (> (num-int v1)(num-int v2))
                (eval-under-env (ifleq-e4 e) env)
                (eval-under-env (ifleq-e3 e) env))
              (error "NUMEX condition applied to non-number")))]

         [(with? e)
          (let ([v1 (eval-under-env (with-e1 e) env)])
            (cond[(string? (with-s e))
                  (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))]
                [else (error "NUMEX with applied to non-string")]))]

         [(closure? e) e]

         [(lam? e)
            (cond[(string? (lam-formal e)) (closure env e)]
            [else (error "NUMEX lam applied to non-string")])]
         
         [(apply? e)
          (let ([fun-closure (eval-under-env (apply-funexp e) env)]) ;evaluating funexp in apply
            ;our funexp is either a closure, or a lam
            (cond[(closure? fun-closure) (let ([fun-def (closure-f fun-closure)]) ;if it is a closure, then 
                                       (let ([eval-actual (eval-under-env (apply-actual e) env)])
                                             (cond[(lam? fun-def)(eval-under-env (lam-body fun-def) (cons (cons (lam-formal fun-def) eval-actual)
                                                                                          (cons (cons (lam-nameopt fun-def) fun-closure) (closure-env fun-closure))))] ;evaluate lam-body under this new enviroment
                                                  [else (error "closure function isn't lam")])))]

                 [(lam? fun-closure) (let* ([lam-closure (eval-under-env fun-closure env)] ;first we evaluate fun-closure to lam-closure
                                           [lam-def (closure-f lam-closure)]) ;if it is a closure, then 
                                       (let ([eval-actual (eval-under-env (apply-actual e) env)])
                                             (cond[(lam? lam-def)(eval-under-env (lam-body lam-def) (cons (cons (lam-formal lam-def) eval-actual)
                                                                                          (cons (cons (lam-nameopt lam-def) lam-closure) (closure-env lam-closure))))] ;evaluate lam-body under this new enviroment
                                                  [else (error "closure function isn't lam")])))]
                 [else (error (format "NUMEX lam apply invalid"))]))]

         
         [(apair? e)
          (let ([v1 (eval-under-env (apair-e1 e) env)]
                [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2))]

      
         [(1st? e)
          (let ([v (eval-under-env (1st-e e) env)])
            (if (apair? v)
              (apair-e1 v)
              (error "NUMEX 1st applied to non-apair")))]

         [(2nd? e)
          (let ([v (eval-under-env (2nd-e e) env)])
            (if (apair? v)
              (apair-e2 v)
              (error "NUMEX 2st applied to non-apair")))]
         
         [(munit? e) e]

         [(ismunit? e)
          (let ([v (eval-under-env (ismunit-e e) env)])
            (if (munit? v)
              (bool #t)
              (bool #f)))]

       

         [(letrec? e)
          (if (and (string? (letrec-s1 e))(string? (letrec-s2 e)) (string? (letrec-s3 e)))
            (eval-under-env (letrec-e4 e) (cons (cons (letrec-s3 e) (letrec-e3 e))
                                                (cons (cons (letrec-s2 e) (letrec-e2 e))
                                                      (cons (cons (letrec-s1 e) (letrec-e1 e)) env))))
            (error "NUMEX lecrec applied to non-string"))]




         [(queue? e)
          (let ([v1 (eval-under-env (queue-e e) env)]
                [v2 (eval-under-env (queue-q e) env)])
            (cond[(or (munit? v2) (queue? v2))(queue v1 v2)]
              [#t(error "NUMEX queue applied to non-queue")]))]
         
        [(enqueue? e)
          (let ([v1 (eval-under-env (enqueue-e e) env)]
                [v2 (eval-under-env (enqueue-q e) env)])
            (cond[(queue? v2) (queue v1 v2)]
              [#t (error "NUMEX enqueue applied to non-queue")]))]
        
        [(dequeue? e)
          (let ([v (eval-under-env (dequeue-q e) env)])
            (cond[(queue? v)
              (if (munit? (queue-q v))
                (munit)
                (eval-under-env (queue (queue-e v) (dequeue (queue-q v))) env))]
              [(error "NUMEX dequeue applied to non-queue")]))]
        
        [(extract? e)
          (let ([v (eval-under-env (extract-q e) env)])
            (cond[(queue? v)
              (if (munit? (queue-q v))
                  (eval-under-env (queue-e v) env)
                  (eval-under-env (extract (queue-q v)) env))]
              [#t(error "NUMEX extract applied to non-queue")]))]
        
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


