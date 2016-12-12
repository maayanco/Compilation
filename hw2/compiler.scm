(include "pattern-matcher.scm")
          
(define const?
  (lambda (val)
    (or
     (equal? val `,(void))
     (equal? val ''())
     (vector? val)
     (boolean? val)
     (char? val)
     (number? val)
     (string? val)
     )))

(define quote?
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

(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (cdr s))
			(else `(begin ,@s)))))

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
          


(define parse-2
  (let ((run
         (compose-patterns
          (pattern-rule ;;const
           (? 'c const?) (lambda (c) `(const ,c)))
          (pattern-rule ;;const - quote
           (? 'c quote?) (lambda (c) `(const ,@(cdr c))))
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
             (let ((identified-lambda (identify-lambda args (lambda (s) `(simple ,s)) (lambda (s opt) `((required ,s) (opt ,opt))) (lambda (var) `(var ,var)))))
             (if (equal? exprs-lst '())
                 (expand-lambda identified-lambda exprs)
                 (expand-lambda identified-lambda (cons 'begin (cons exprs exprs-lst)))
             ))))
            ; `(lambda ,args ,exprs))) 
           (pattern-rule ;;define MIT
            `(define ( ,(? `var1) . ,(? `vars-lst) ) ,(? `exp1) . ,(? `exps-lst))
            (lambda (var1 vars-lst exp1 exps-lst)
             (if (equal? exps-lst `()) 
              `(def (var ,var1) ,(parse-2 `(lambda ,vars-lst ,exp1)))
              `(def (var ,var1)  ,(parse-2 `(lambda ,vars-lst ,exp1 ,@exps-lst))))))
           (pattern-rule ;;define
            `(define ,(? `var) ,(? `exp))
            (lambda (var exp)  `(def (var ,var) ,(parse-2 exp))))

           (pattern-rule ;;set!
            `(set! ,(? `var) ,(? `exp))
            (lambda (var exp) `(set (var ,var) ,(parse-2 exp) )))
           (pattern-rule ;;sequence
            `(begin ,(? 'val1) . ,(? `vals-lst))
            (lambda (val1 vals-lst)
              `(seq ( ,(parse-2 val1)
                      ,@(map
                         
                      (lambda (item) (let ((parsed-item (parse-2 item)))
                                       (if seq? parsed-item)
                                       ((lambda ()
                                          (cadr parsed-item)))
                                       parsed-item))
                      vals-lst)))))
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
                                       ; (begin ;(display (list (car item) (cons `begin (cdr item))))
                                              ; (display "||\n")
                                               (list (car item) (cons `begin (cdr item)))
                                               item))
                                      (cons expr exprs-lst))))
               ; (display "exprs:") (display new-exprs) (display "end-exprs")
            ;    (map (lambda (item) (display "{") (display item) (display "}\n")) exprs)
                (parse-2 (nested-if-cond new-exprs))
                )))
           (pattern-rule ;;let
            `(let () ,(? `exprs-lst))
            (lambda (exprs-lst)
              (parse-2 `((lambda () ,exprs-lst)))))
           (pattern-rule
            `(let ((,(? 'var var?) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
            (lambda (var val rest exprs)
              (let ((vars (cons var (map car rest)))
                    (vals (cons val (map cadr rest))))

               (parse-2 `((lambda (,@vars) ,@exprs) ,@vals)))))
		(pattern-rule ;;let*
                `(let* () ,(? 'expr) . ,(? 'exprs list?))
                (lambda (expr exprs)
                  (parse-2 (cons 'begin (cons expr exprs)))))
		(pattern-rule
                 `(let* ((,(? 'var) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
                 (lambda (var val rest exprs) (parse `(let ((,var val)) (let* ,rest . ,exprs)))))
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
            