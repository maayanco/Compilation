
(include "pattern-matcher.scm")

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


(define not-empty?
  (lambda (lst)
    (if (equal? lst `()) #f #t)))

(define const2?
  (lambda (val)
    (or
     (equal? val `,(void))
     ;(equal? val ''())
     (equal? val '())
     (vector? val)
     (boolean? val)
     (char? val)
     (number? val)
     (string? val)
     )))

(define quote1?
  (lambda (val)
    (if (list? val)
        (if (equal? (car val) 'quote) #t #f)
        #f)))
    
;; VARIABLES

(define var?
  (lambda (val)
    (not (is-reserved? val))
    ))

  (define is-reserved?
    (lambda (val)
      (ormap (lambda (item) (equal? val item)) *reserved-words*)
      ))
(define not-is-reserved?
    (lambda (val)
      (not (ormap (lambda (item) (equal? val item)) *reserved-words*))
      ))
 
    
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))



(define var?
  (lambda (item)
    (and
     (not-is-reserved? item)
     (not (equal? item '()))
     (not (pair? item)) )))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))

(define expand-lambda
  (lambda (lambda-ret exprs)
    (let ((parsed-exprs (parse-2 exprs)))
          (cond
            ;;lambda-simple
            ((and (equal? (length lambda-ret) 2 ) (equal? (car lambda-ret) 'simple))
             `(lambda-simple ,(cadr lambda-ret) ,parsed-exprs))
            ;;lambda-var
            ((and (equal? (length lambda-ret) 2) (equal? (car lambda-ret) 'var))
             `(lambda-var ,(cadr lambda-ret) ,parsed-exprs))
            ;;lambda-opt
            ((and (equal? (length lambda-ret) 2) (map pair? lambda-ret) )
             `(lambda-opt ,(cadar lambda-ret) ,(cadadr lambda-ret) ,parsed-exprs))
            ))))
      
        
    ;((required (a b c d)) (opt e))
    ; (simple (a b c))
    ; (var moshe)
       (define seq?
         (lambda (exp)
         (and (list? exp) (equal? (car exp) 'seq))))


(define nested-if ;;for and macro expansion
  (lambda (exprs)
    (cond ((equal? (length exprs) 2) `(if ,(car exprs) ,(cadr exprs) #f))
          (else `(if ,(car exprs) ,(nested-if (cdr exprs)) #f))
    
        )))

(define nested-if-cond ;;for cond macro expansion
  (lambda (exprs) ;;exprs is a list of pairs
    (cond  ((and (not (equal? exprs `())) (list? (car exprs)) (not (equal? (caar exprs) 'else)))
           `(if ,(caar exprs) ,@(cdar exprs) ,(nested-if-cond (cdr exprs))))
           ((and (not (equal? exprs '())) (equal? (caar exprs) 'else)) (cadar exprs)))
    ))

(define beginify
  (lambda (expr)
    (fold-left (lambda (l r)  (if (and (list? r) (equal? `begin (car r)))  (append l (beginify (cdr r))) `(,@l ,r))) '() expr)))

(define check-lambda-params-diff
  (lambda (params)
     ; (display "vars:") (display vars) (display "\n")
      (andmap (lambda (item) ;(display (length (remove item vars)) ) (display "\n") (display (length vars)) (display "\n") 
                (equal? (length (remove item params)) (- (length params) 1) )) 
              params)
    ))

(define check-let-params-diff
  (lambda (params)
    ;(display "params:") (display params) (display "\n")
    (let ((vars (map car params)))
     ; (display "vars:") (display vars) (display "\n")
      (andmap (lambda (item) ;(display (length (remove item vars)) ) (display "\n") (display (length vars)) (display "\n") 
                (equal? (length (remove item vars)) (- (length vars) 1) )) 
              vars))
    ))

(define parse-2
  (let ((run
         (compose-patterns
          (pattern-rule ;;const
           (? 'c const2?) (lambda (c) `(const ,c)))
          (pattern-rule ;;const - quote
           (? 'c quote1?) (lambda (c) `(const ,@(cdr c))))
          (pattern-rule ;; if test dit dif
           `(if  ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (<test> <dit> <dif>)
            `(if3 ,(parse-2 <test>) ,(parse-2 <dit>) ,(parse-2 <dif>))))
          (pattern-rule ;;if test dit 
           `(if  ,(? 'test) ,(? 'dit))
           (lambda (<test> <dit>)
            `(if3 ,(parse-2 <test>) ,(parse-2 <dit>) ,(parse-2 `,(void)))))
          (pattern-rule
           `(or) (lambda () (parse-2 #f)))
          (pattern-rule ;;or
           `(or  ,(? 'expr) . ,(? 'lst list?))
           (lambda (expr lst)
             (cond
               ((equal? lst `()) (parse-2 expr))
               (else `(or ,(cons (parse-2 expr)  (map parse-2 lst) ) )))))
          (pattern-rule ;;lambda
           `(lambda ,(? `args) ,(? `exprs) . ,(? `exprs-lst))
           (lambda (args exprs exprs-lst)
             (if (and (list? args) (not (check-lambda-params-diff args)))
                 (error 'parse  (format "I can't recognize this: ~s" args))
                 (let ((identified-lambda (identify-lambda args (lambda (s) `(simple ,s)) (lambda (s opt) `((required ,s) (opt ,opt))) (lambda (var) `(var ,var)))))
                   (if (equal? exprs-lst '())
                       (expand-lambda identified-lambda exprs)
                       (expand-lambda identified-lambda (cons 'begin (cons exprs exprs-lst)))
                       ))
                 )
             ))
            (pattern-rule ;;quasi-quote
             `(quasiquote . ,(? 'args))
             (lambda (args)
               ;(display (car args)) (display "||\n")
               (parse-2 (expand-qq (car args)))))
            
           (pattern-rule ;;define MIT
            `(define ( ,(? `var1) . ,(? `vars-lst) ) ,(? `exp1) . ,(? `exps-lst))
            (lambda (var1 vars-lst exp1 exps-lst)
             (if (equal? exps-lst `()) 
              `(def (var ,var1) ,(parse-2 `(lambda ,vars-lst ,exp1)))
              `(def (var ,var1)  ,(parse-2 `(lambda ,vars-lst ,exp1 ,@exps-lst))))))
           (pattern-rule
            `(define ,(? `var) . ,(? `vars list?))
            (lambda (var vars)
              (if (equal? vars `())
                   (error 'parse
		    (format "Unknown form: ~s" (cons `define var) ))
                   `(def (var ,var) ,(parse-2 (cons `begin vars))))
                  
              ))
           (pattern-rule ;;define
            `(define ,(? `var) ,(? `exp))
            (lambda (var exp)  `(def (var ,var) ,(parse-2 exp))))

           (pattern-rule ;;set!
            `(set! ,(? `var) ,(? `exp))
            (lambda (var exp) `(set (var ,var) ,(parse-2 exp) )))
           (pattern-rule
            `(begin)
            (lambda ()
              (parse-2 `,(void))))
           (pattern-rule 
            `(begin  ,(? `expr))
            (lambda (expr) (parse-2 expr)))
           (pattern-rule 
            `(begin . ,(? `lst list?)) 
            (lambda (lst) `(seq ,(map parse-2 (beginify lst)))))

           (pattern-rule ;;and
            `(and)
            (lambda () (parse-2 #t)))
           (pattern-rule
            `(and ,(? `expr) . ,(? `exprs))
            (lambda (expr exprs)
              (if (equal? exprs `())
                  (parse-2 expr)
                  (parse-2 (nested-if (cons expr exprs))))))
           (pattern-rule
            `(cond ,(? `expr) . ,(? `exprs-lst))
            (lambda (expr exprs-lst)
              (let 
                  ((new-exprs (map (lambda (item) (if (and (list? (cdr item)) (not (equal? (length (cdr item)) 1)))
                                               (list (car item) (cons `begin (cdr item)))
                                               item))
                                      (cons expr exprs-lst))))
                (parse-2 (nested-if-cond new-exprs))
                )))
           (pattern-rule ;;let
            `(let () ,(? 'expr) . ,(? 'exprs list?))
            (lambda (expr exprs)
              (if (not-empty? exprs)
                  (parse-2 `((lambda () ,(cons `begin (cons expr exprs)))))
                  (parse-2 `((lambda () ,@(cons expr exprs))))
              )))
           
           (pattern-rule
            `(let ((,(? 'var var?) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
            (lambda (var val rest exprs)
              (if (check-let-params-diff (cons (cons var val) rest))
                  (let ((vars (cons var (map car rest)))
                        (vals (cons val (map cadr rest))))
                    (parse-2 `((lambda (,@vars) ,@exprs) ,@vals)))
                  (error 'parse
		    (format "Invalid parameter list: ~s" (map car (cons (cons var val) rest)))))))
           
		(pattern-rule ;;let*
                `(let* () ,(? 'expr) . ,(? 'exprs list?))
                (lambda (expr exprs)
                  (if (not-empty? exprs)
                      (parse-2 `(let () ,(cons `begin (cons expr exprs))))
                      (parse-2 `(let () ,expr)))
                  ))
		(pattern-rule
                 `(let* ((,(? 'var) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
                 (lambda (var val rest exprs)
                   (if (and (equal? (length rest) 0) )
                       (if (equal? (length exprs) 1)
                          (parse-2 `(let ((,var ,val)) ,(car exprs)))
                           (parse-2 `(let ((,var ,val)) ,(cons `begin exprs))))
                        (parse-2 `(let ((,var ,val)) (let* ,rest ,@exprs))))
                   ))
                (pattern-rule ;;letrec
                 `(letrec () ,(? 'expr) . ,(? 'exprs list?))
                 (lambda (expr exprs)
                   (if (not-empty? exprs)
                       (begin (display "yyy") (parse-2 `(let () ,(cons `begin `((lambda () ,(list `begin expr exprs))) ))))
                      (begin (display "ooo") (parse-2 `(let () ((lambda () ,expr))))))
                   ))
                (pattern-rule
                 `(letrec ((,(? 'var) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
                 (lambda (var val rest exprs)
                   (display "exprs:") (display exprs) (display "\n")
                   (let ((new-exprs (if (equal? (length exprs) 1) (car exprs) exprs)))
                     (display "new-exprs:") (display new-exprs) (display "\n")
                     (if (equal? `() rest)
                         ;(parse-2 `((lambda () ,(cons expr exprs))
                         (begin (display "ooops")
                                ;(parse-2 `(let ((,var #f)) ,(list `begin `(set! ,var ,val) `(let () ,new-exprs))))
                                (parse-2 `(let ((,var #f)) ,(list `begin `(set! ,var ,val) `((lambda () ,new-exprs))))))
                         (let ((new-rest (map (lambda (pair) (list (car pair) #f)) rest))
                               (new-sets (map (lambda (pair) (cons `set! pair)) rest)))
                           (begin (display "new-rest: ") (display new-rest) (display "\n")
                                  (display "new-sets: ") (display new-sets) (display "\n")
                                  (display "something else:") (display (cons `(set! ,var #f) new-sets)) (display "\n")
                                  (parse-2 `(let ,(cons (list var #f) new-rest) ,(append (cons `begin (cons `(set! ,var ,val) new-sets)) `((let () ,@exprs))))))))
                             ;(parse-2 `(let ((,var #f)) ,(list `begin `(set! ,var #f) new-sets `(let () ,@exprs)))))
                     )))
                
           (pattern-rule ;;application
            `( ,(? `var not-is-reserved?) . ,(? `vars-lst))
            (lambda (var vars-lst) `(applic ,(parse-2 var) ,(map parse-2 vars-lst))))
           
          (pattern-rule
           (? 'v var?) (lambda (v) `(var ,v)))


          
          )))
         (lambda (e) (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))