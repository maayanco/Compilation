(include "pattern-matcher.scm")
          
(define const?
  (lambda (val)
    (or
     (equal? val `())
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


(define parse
  (let ((run
         (compose-patterns
          (pattern-rule ;;const
           (? 'c const?) (lambda (c) `(const ,c)))
          (pattern-rule ;;const - quote
           (? 'c quote?) (lambda (c) `(const ,@(cdr c))))
          (pattern-rule ;; if test dit dif
           `(if  ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (<test> <dit> <dif>)
            `(if3 ,(parse <test>) ,(parse <dit>) ,(parse <dif>))))
          (pattern-rule ;;if test dit 
           `(if  ,(? 'test) ,(? 'dit))
           (lambda (<test> <dit>)
            `(if3 ,(parse <test>) ,(parse <dit>) ,(parse (void)))))
          (pattern-rule ;;or
           `(or  ,(? 'expr) . ,(? 'lst list?))
           (lambda (expr lst)  `(or ,(cons (parse expr)  (map parse lst) ) )))
           (pattern-rule ;;lambda with optional args
            `(lambda ( ,(? `arg1) ,(? `original-args-lst)  ,(? `rest-args) ) ,(? `exp1)  ,(? `exps-lst))
            (lambda (arg1 original-args-lst dot rest-args exp1 exps-lst) (display "hi guys")))
          (pattern-rule ;;simple-lambda
            `(lambda (,(? `args) . ,(? `args-lst) ) ,(? `exp1) . ,(? `exps) )
            (lambda (args args-lst exp1 exps) `(lambda-simple (,args ,@args-lst)  ,(parse `(begin ,exp1 ,@exps)))))

           (pattern-rule ;;define MIT
            `(define ( ,(? `var1) . ,(? `vars-lst) ) ,(? `exp1) . ,(? `exps-lst))
            (lambda (var1 vars-lst exp1 exps-lst)
             (if (equal? exps-lst `()) 
              `(define (var ,var1) ,(parse `(lambda ,vars-lst ,exp1)))
              `(define (var ,var1)  ,(parse `(lambda ,vars-lst ,exp1 ,@exps-lst))))))
           (pattern-rule ;;define
            `(define ,(? `var) ,(? `exp))
            (lambda (var exp)  `(define (var ,var) ,(parse exp))))

           (pattern-rule ;;set!
            `(set! ,(? `var) ,(? `exp))
            (lambda (var exp) `(set (var ,var) ,(parse exp) )))
           (pattern-rule ;;sequence
            `(begin ,(? 'val1) . ,(? `vals-lst))
            (lambda (val1 vals-lst) `(seq ( ,(parse val1) ,@(map parse vals-lst)))))
           (pattern-rule ;;application
            `( ,(? `var not-is-reserved?) . ,(? `vars-lst))
            (lambda (var vars-lst) `(applic ,(parse var) ,(map parse vars-lst))))
           
          (pattern-rule
           (? 'v var?) (lambda (v) `(var ,v)))
             ;(map (lambda (expr) (parse expr)) list-exprs)))
          ;(pattern-rule
          ;(? 'v var?) (lambda (v) (if (symbol? v)
          ;                           `(var ,(string->list (symbol->string v)))
          ;                            `(var ,@v))))
          
          )))
         (lambda (e) (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))
            