
(include "pc.scm")


;; code from mayer's uploaded file
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new 	(*parser (char #\;))
	 
	 		(*parser <any-char>)
	 		(*parser <end-of-line-comment>)
	 		*diff 
	 		*star

	 		(*parser <end-of-line-comment>)
	 		(*caten 3)
	done)))


(define <sexpr-comment>
  (new  (*parser (word "#;"))
	    (*delayed (lambda () <sexpr>))
        (*caten 2)
        done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define <infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

(define <comment-infix>
  (disj <line-comment>
	<infix-comment>))

(define <skip-infix>
  (disj <comment-infix>
	<whitespace>))

(define ^<skipped-infix*> (^^<wrapped> (star <skip-infix>)))

(define <digit-0-9> (range #\0 #\9))
(define <digit-1-9> (range #\1 #\9))

(define <Boolean> 
	(new
		(*parser (word-ci "#t"))
		(*pack (lambda (_) #t))
		(*parser (word-ci "#f"))
		(*pack (lambda (_) #f))
		(*disj 2)
		done))


(define <CharPrefix> 
	(new	(*parser (char #\#))
			(*parser (char #\\)) 
		    (*caten 2)
		    (*pack-with (lambda (a b) #\\))
			done))


(define <VisibleSimpleChar> 
	(new (*parser (range #\! (integer->char 255)))
		 done))


(define <NamedChar>
	(new
		(*parser (word-ci "lambda"))
		(*pack (lambda (_) (integer->char 955)))
		(*parser (word-ci "newline"))
		(*pack (lambda (_) `#\newline))
		(*parser (word-ci "nul"))
		(*pack (lambda (_) `#\nul))
		(*parser (word-ci "page"))
		(*pack (lambda (_) `#\page ))
		(*parser (word-ci "return"))
		(*pack (lambda (_) `#\return ))
		(*parser (word-ci "space"))
		(*pack (lambda (_) `#\space ))
		(*parser (word-ci "tab"))
		(*pack (lambda (_) `#\tab ))
		(*disj 7)
		done))


(define <HexChar>
	(new (*parser (range #\0 #\9) )
		(*parser (range #\a #\f) )
		(*disj 2)
		(*pack (lambda (a)  a))
		done))


(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*pack (lambda (_) `#\x))
		(*parser (char #\X))
		(*pack (lambda (_) `#\x))
		(*disj 2)
		(*parser <HexChar>) *plus
		(*caten 2)
		(*pack-with (lambda (a b) (if (< (string->number (list->string `(,@b) )  16) 1114112)
									(integer->char (string->number (list->string `(,@b) )  16))
									"failed-to-convert-input")))
		done))


(define <Char>
	(new 
		(*parser <CharPrefix>)
		(*parser <NamedChar>)
		(*parser <HexUnicodeChar>)
		(*guard (lambda (a) (not (equal? a "failed-to-convert-input"))))
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 2)
		(*pack-with (lambda (a b) `( ,@b) ))
		done))


(define <StringLiteralChar> 
	(new 
		(*parser <any-char>)
		(*parser (char #\\))
		*diff
		done))


(define <StringHexChar>
	(new 
		(*parser (word "\\"))
		(*parser (word-ci "x"))
		(*parser <HexChar>) *star
		(*parser (word ";"))
		(*caten 4)
		(*pack-with (lambda (pre x b comma) 
									(if (< (string->number (list->string `(,@b) )  16) 1114112)
										(integer->char (string->number (list->string `(,@b) )  16))
										#\space)))
		(*guard (lambda (a) (not (equal? a #\space)) ))
		done))


(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) 
       (*disj 6)
       done))



(define <string-char>
  (new        
        (*parser <StringMetaChar>)
		(*parser <StringHexChar>)
		(*parser <StringLiteralChar>)
		(*parser (char #\"))
        *diff
		(*disj 3)
        done))

(define <String>
  (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)
       (*pack-with (lambda (open-delim chars close-delim) (list->string chars)))
       done))


(define <SymbolChar>
	(new 
		(*parser (range #\0 #\9))
		(*parser (range #\a #\z))
		(*parser (range #\A #\Z))
		(*pack (lambda (a) (integer->char (+ (char->integer a) 32))))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\<))
		(*parser (char #\>))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 15)
	done))


(define <Symbol>
	(new 
		(*parser <SymbolChar>) *plus
		(*pack (lambda (a) (string->symbol (list->string a))))
		done))

		
(define <Natural>
	(new
		(*parser <digit-0-9>) *plus
		(*pack (lambda (a) (string->number (list->string a)) ))
		done))

(define <Integer>
	(new 
		(*parser (word "+"))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (++ n) n))

		(*parser (word "-"))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (-- n ) (- n)))

		(*parser <Natural>)

		(*disj 3)
		done))
       
 (define <Fraction>
  (new (*parser <Integer>)
  	   (*parser <whitespace>)
  		*not-followed-by
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with (lambda (a div b) (/ a b)))
       done))
		
(define <Number>
    (new 
         (*parser <Fraction>)
         (*parser <Integer>)
         (*disj 2)
         done))

(define <OnlyNumberInfix>
      (new (*parser <Fraction>)
           (*parser <Integer> )
           (*parser (range #\a #\z))
           (*parser (range #\A #\Z))
           (*disj 2)
           *not-followed-by
           (*disj 2)done))


(define <SymbolNotNumber>
  (new (*parser <Symbol>)
       (*parser (range #\0 #\9))
       *diff
       done))


(define <OnlyNumberSexpr>
        (new (*parser (not-followed-by <Number> <SymbolNotNumber>))
              done))



(define <ProperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr>) ) *star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre a suf) a ))
		done))


(define <ImproperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr>) ) *plus
		(*parser (word "."))
		(*delayed (lambda () <sexpr>) )
		(*parser (word ")"))
		(*caten 5)
		(*pack-with (lambda (brk1 exp1 point exp2 brk2) `(,@exp1 . ,exp2) ))
		done))

(define <Vector>
	(new
		(*parser (word "#("))
		(*delayed (lambda () <sexpr>) )
        *star
        (*pack (lambda (a) a))
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre exp suf) (list->vector exp)  ))
		done))

(define <Quoted>
	(new
		(*parser (word "'"))
		(*delayed (lambda () <sexpr>) )
		(*caten 2)
		(*pack-with (lambda (pre exp) (list 'quote exp)))
		done))

(define <QuasiQuoted>
	(new
		(*parser (word "`"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (pre exp) (list 'quasiquote exp)))
		done))

(define <Unquoted>
	(new
		(*parser (word ","))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (pre exp) (list 'unquote exp)))
		done))

(define <UnquoteAndSpliced>
	(new
		(*parser (word ",@"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (pre exp) (list 'unquote-splicing exp) ))
		done))


(define <InfixUndesirables>
	(new
		(*parser (char #\+))
		(*parser (char #\)))
		(*parser (char #\())
		(*parser (char #\[))
		(*parser (char #\]))
		(*parser (char #\-))
		(*parser (char #\*))
		(*parser (word-ci "**" ))
		(*parser (char #\^))
		(*parser (char #\/))
	(*disj 10)
	done))

(define <InfixSymbol> 
		(^<skipped-infix*>
	(new 
		(*parser <SymbolChar>)
		(*pack (lambda (a) `(,@a)))
		(*parser <InfixUndesirables>)
		*diff
		*plus
		(*pack (lambda (a) (string->symbol (list->string a))))
		
	done)))

(define apply-func (lambda (el func) (func el)))

(define <BasicExpression>
	(^<skipped-infix*>
(new 
     (*delayed (lambda () <InfixSexprEscape>))
	 (*parser <OnlyNumberInfix>)
	 (*delayed (lambda () <InfixNeg>))
	 (*delayed (lambda () <InfixParen>))
	 (*parser <InfixSymbol>)
	 (*disj 5)
	 (*delayed (lambda ()<InfixArrayGet>))
	 (*delayed (lambda () <InfixFuncall>))
	 (*disj 2)
	 *star
	 (*caten 2)
	 (*pack-with (lambda (el func) (fold-left apply-func el func)))
	 done)))

(define <InfixPrefixExtensionPrefix>
	(new (*parser (word "##"))
		 (*parser (word "#%"))
		 (*disj 2)
	done))


(define <InfixAddOrSub>
(^<skipped-infix*>
(new (*delayed (lambda () <InfixMultOrDiv>))
     (*parser (char #\+))
	 (*parser (char #\-))
	 (*disj 2)
	 (*delayed (lambda () <InfixMultOrDiv>))
	 (*caten 2)
	 (*pack-with (lambda (sign exp2) (lambda (exp1) (list (string->symbol (string sign)) exp1 exp2)))) *star
	 (*caten 2)
	 (*pack-with (lambda (el func) (fold-left apply-func el func)))
done)))


(define <InfixNeg>
		(^<skipped-infix*>
	(new (*parser (word "-"))
		 (*delayed (lambda () <BasicExpression>) )
		 (*caten 2)
		 (*pack-with (lambda (minus exp) `(- ,exp)))
	done)))


;; ret
(define <InfixMultOrDiv>
		(^<skipped-infix*>
	(new (*delayed (lambda () <InfixPow>))
		 (*parser (char #\*))
		 (*parser (char #\/))
		 (*disj 2)
		 (*delayed (lambda () <InfixPow>))
		 (*caten 2)
		 (*pack-with (lambda (sign exp2) (lambda (exp1) (list (string->symbol (string sign)) exp1 exp2)))) *star
		 (*caten 2)
		 (*pack-with (lambda (el func) (fold-left apply-func el func)))
	done)))



(define <PowerSymbol>
		(^<skipped-infix*>
	(new (*parser (word "^"))
		 (*pack (lambda (a) (string->symbol "^")))
		 (*parser (word "**"))
		 (*pack (lambda (a) (string->symbol "**")))
		 (*disj 2)
	done)))

(define <InfixPow>
		(^<skipped-infix*>
	(new (*parser <BasicExpression>)
		 (*parser <PowerSymbol>)
		 (*delayed (lambda () <InfixPow>))
		 (*parser <BasicExpression>)
		 (*disj 2)
		 (*caten 2)
		 (*pack-with (lambda (sign exp2) (lambda (exp1) `(expt ,exp1 ,exp2)))) *star
		 (*caten 2)
		 (*pack-with (lambda (el func) (fold-left apply-func el func)))
	done)))


(define <InfixArgList>
		(^<skipped-infix*>
	(new 
		 (*delayed (lambda () <InfixAddOrSub>))

		 (*parser (word ","))
		 (*delayed (lambda () <InfixAddOrSub>)) 
		 (*caten 2)
		 (*pack-with (lambda (comma exp) exp ))
		 *star
		 (*caten 2)
		 (*pack-with (lambda (exp1 exp2) (cons exp1 exp2) ))
	done)))


(define <InfixFuncall> 
		(^<skipped-infix*>
	(new 
		(*parser <whitespace>) *star
		(*parser (word "("))
		(*delayed (lambda () <InfixArgList>))
		(*parser (word ")"))
		(*parser <whitespace>) *star
		(*caten 5)
		(*pack-with (lambda (spc1 brk1 exp2 brk2 spc2) (lambda (exp1) (cons exp1 exp2) )))

		(*parser <whitespace>) *star
		(*parser (word "("))
		(*parser <whitespace>) *star
		(*parser (word ")"))
		(*parser <whitespace>) *star
		(*caten 5)
		 (*pack-with (lambda (spc1 brk1 spcM brk2 spc2) (lambda (exp1) (cons exp1 '())))) 
		(*disj 2)
		
    done)))


(define <InfixArrayGet> 
		(^<skipped-infix*>
	(new 
		(*parser <whitespace>) *star
		(*parser (word "["))
		(*delayed (lambda () <InfixAddOrSub>))
		(*parser (word "]"))
		(*parser <whitespace>) *star
		(*caten 5)
		(*pack-with (lambda (spc1 brk1 exp2 brk2 spc2) (lambda (exp1) (list 'vector-ref exp1 exp2) )))
    done)))

(define <InfixParen>
		(^<skipped-infix*>
	(new (*parser (word "("))
		 (*delayed (lambda () <InfixAddOrSub>))
		 (*parser (word ")"))
		 (*caten 3)
		 (*pack-with (lambda (brk1 exp brk2) exp ))
	done)))

(define <InfixSexprEscape>
		(^<skipped-infix*>
	(new (*parser <InfixPrefixExtensionPrefix>)
		 (*delayed (lambda () <sexpr>))
		 (*caten 2)
		 (*pack-with (lambda (pre exp) exp ))
	done)))


(define <InfixExpression>
(^<skipped-infix*>
	(new
		(*parser <InfixAddOrSub>)
		(*parser <InfixSexprEscape>)
		(*disj 2)
		done)))

(define <InfixExtension>
	(^<skipped-infix*>
	(new (*parser <InfixPrefixExtensionPrefix>)
		 (*parser <InfixExpression>)
		 (*caten 2)
		 (*pack-with (lambda (sign exp) exp))
		 done)))


(define <sexpr>
	(^<skipped*>
	(new
		(*parser <InfixExtension>)
		(*parser <ImproperList>)
		(*parser <ProperList>)
		(*parser <Vector>)
		(*parser <Boolean>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <Unquoted>)
		(*parser <UnquoteAndSpliced>)
		(*parser <OnlyNumberSexpr>)
		(*parser <Char>)
		(*parser <Symbol>)
		(*parser <String>)
		(*disj 13)
		done)))

;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGNMENT 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let ((parsed-exprs (parse exprs)))
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
      (andmap (lambda (item) 
                (equal? (length (remove item params)) (- (length params) 1) )) 
              params)
    ))

(define check-let-params-diff
  (lambda (params)
    (let ((vars (map car params)))
      (andmap (lambda (item)  
                (equal? (length (remove item vars)) (- (length vars) 1) )) 
              vars))
    ))

(define parse
  (let ((run
         (compose-patterns
          (pattern-rule ;;const
           (? 'c const2?) (lambda (c) `(const ,c)))
          (pattern-rule ;;const - quote
           (? 'c quote1?) (lambda (c) `(const ,@(cdr c))))
          (pattern-rule ;; if test dit dif
           `(if  ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (<test> <dit> <dif>)
            `(if3 ,(parse <test>) ,(parse <dit>) ,(parse <dif>))))
          (pattern-rule ;;if test dit 
           `(if  ,(? 'test) ,(? 'dit))
           (lambda (<test> <dit>)
            `(if3 ,(parse <test>) ,(parse <dit>) ,(parse `,(void)))))
          (pattern-rule
           `(or) (lambda () (parse #f)))
          (pattern-rule ;;or
           `(or  ,(? 'expr) . ,(? 'lst list?))
           (lambda (expr lst)
             (cond
               ((equal? lst `()) (parse expr))
               (else `(or ,(cons (parse expr)  (map parse lst) ) )))))
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
               (parse (expand-qq (car args)))))
            
           (pattern-rule ;;define MIT
            `(define ( ,(? `var1) . ,(? `vars-lst) ) ,(? `exp1) . ,(? `exps-lst))
            (lambda (var1 vars-lst exp1 exps-lst)
             (if (equal? exps-lst `()) 
              `(def (var ,var1) ,(parse `(lambda ,vars-lst ,exp1)))
              `(def (var ,var1)  ,(parse `(lambda ,vars-lst ,exp1 ,@exps-lst))))))
           (pattern-rule
            `(define ,(? `var) . ,(? `vars list?))
            (lambda (var vars)
              (if (equal? vars `())
                   (error 'parse
		    (format "Unknown form: ~s" (cons `define var) ))
                   `(def (var ,var) ,(parse (cons `begin vars))))
                  
              ))
           (pattern-rule ;;define
            `(define ,(? `var) ,(? `exp))
            (lambda (var exp)  `(def (var ,var) ,(parse exp))))

           (pattern-rule ;;set!
            `(set! ,(? `var) ,(? `exp))
            (lambda (var exp) `(set (var ,var) ,(parse exp) )))
           (pattern-rule
            `(begin)
            (lambda ()
              (parse `,(void))))
           (pattern-rule 
            `(begin  ,(? `expr))
            (lambda (expr) (parse expr)))
           (pattern-rule 
            `(begin . ,(? `lst list?)) 
            (lambda (lst) `(seq ,(map parse (beginify lst)))))

           (pattern-rule ;;and
            `(and)
            (lambda () (parse #t)))
           (pattern-rule
            `(and ,(? `expr) . ,(? `exprs))
            (lambda (expr exprs)
              (if (equal? exprs `())
                  (parse expr)
                  (parse (nested-if (cons expr exprs))))))
           (pattern-rule
            `(cond ,(? `expr) . ,(? `exprs-lst))
            (lambda (expr exprs-lst)
              (let 
                  ((new-exprs (map (lambda (item) (if (and (list? (cdr item)) (not (equal? (length (cdr item)) 1)))
                                               (list (car item) (cons `begin (cdr item)))
                                               item))
                                      (cons expr exprs-lst))))
                (parse (nested-if-cond new-exprs))
                )))
           (pattern-rule ;;let
            `(let () ,(? 'expr) . ,(? 'exprs list?))
            (lambda (expr exprs)
              (if (not-empty? exprs)
                  (parse `((lambda () ,(cons `begin (cons expr exprs)))))
                  (parse `((lambda () ,@(cons expr exprs))))
              )))
           
           (pattern-rule
            `(let ((,(? 'var var?) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
            (lambda (var val rest exprs)
              (if (check-let-params-diff (cons (cons var val) rest))
                  (let ((vars (cons var (map car rest)))
                        (vals (cons val (map cadr rest))))
                    (parse `((lambda (,@vars) ,@exprs) ,@vals)))
                  (error 'parse
		    (format "Invalid parameter list: ~s" (map car (cons (cons var val) rest)))))))
           
		(pattern-rule ;;let*
                `(let* () ,(? 'expr) . ,(? 'exprs list?))
                (lambda (expr exprs)
                  (if (not-empty? exprs)
                      (parse `(let () ,(cons `begin (cons expr exprs))))
                      (parse `(let () ,expr)))
                  ))
		(pattern-rule
                 `(let* ((,(? 'var) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
                 (lambda (var val rest exprs)
                   (if (and (equal? (length rest) 0) )
                       (if (equal? (length exprs) 1)
                          (parse `(let ((,var ,val)) ,(car exprs)))
                           (parse `(let ((,var ,val)) ,(cons `begin exprs))))
                        (parse `(let ((,var ,val)) (let* ,rest ,@exprs))))
                   ))
                (pattern-rule ;;letrec
                 `(letrec () ,(? 'expr) . ,(? 'exprs list?))
                 (lambda (expr exprs)
                   (if (not-empty? exprs)
                       (begin (parse `(let () ,(cons `begin `((lambda () ,(list `begin expr exprs))) ))))
                      (begin (parse `(let () ((lambda () ,expr))))))
                   ))
                (pattern-rule
                 `(letrec ((,(? 'var) ,(? `val)) . ,(? 'rest)) . ,(? 'exprs))
                 (lambda (var val rest exprs)
                   (let ((new-exprs (if (equal? (length exprs) 1) (car exprs) exprs)))
                     (if (equal? `() rest)
                         (begin 
                                (parse `(let ((,var #f)) ,(list `begin `(set! ,var ,val) `((lambda () ,new-exprs))))))
                         (let ((new-rest (map (lambda (pair) (list (car pair) #f)) rest))
                               (new-sets (map (lambda (pair) (cons `set! pair)) rest)))
                           (begin 
                                  (parse `(let ,(cons (list var #f) new-rest) ,(append (cons `begin (cons `(set! ,var ,val) new-sets)) `((let () ,@exprs))))))))
                     )))
                
           (pattern-rule ;;application
            `( ,(? `var not-is-reserved?) . ,(? `vars-lst))
            (lambda (var vars-lst) `(applic ,(parse var) ,(map parse vars-lst))))
           
          (pattern-rule
           (? 'v var?) (lambda (v) `(var ,v)))


          
          )))
         (lambda (e) (run e
	   (lambda ()
	     (error 'parse
		    (format "I can't recognize this: ~s" e)))))))						


;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGNMENT 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;  HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define get-const-val
	(lambda (expr)
	(car expr)))

(define get-value-of-var
	(lambda (expr)
	(car expr)))

(define get-tatey-expr
	(lambda (expr)
	(car expr)))

(define get-applic-operator
	(lambda (expr)
	(car expr)))

(define get-applic-operands
	(lambda (expr)
	(cadr expr)))
	
(define get-or-sub-exprs
	(lambda (expr)
		(car expr)))

(define get-define-var
	(lambda (expr)
	(car expr)))

(define get-define-val
	(lambda (expr)
	(cadr expr)))
	
(define get-set-var
	(lambda (expr)
	(car expr)))

(define get-set-val
	(lambda (expr)
	(cadr expr)))

(define get-if-condition
	(lambda (expr)
	(car expr)))

(define get-if-first
	(lambda (expr)
	(cadr expr)))

(define get-if-second
	(lambda (expr)
		(caddr expr)))
		
(define get-lambda-parameters
	(lambda (expr)
		(car expr)))
		
(define get-lambda-opt-params
	(lambda (expr)
		(cadr expr)))
	
(define get-lambda-body
	(lambda (expr)
		(cadr expr)))
		
(define get-lambda-opt-body
	(lambda (expr)
		(caddr expr)))

(define list-reverse
	(lambda (lst)
	  (if (null? lst) lst 
		 (append (list-reverse (cdr lst)) (list (car lst)))
	  )
	)
)
	
(define add-sograim
	(lambda (param) (list param))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;END OF HELPER FUNCTIONS;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;TASK 3 - START ;;;;;;;;;;;;;;;;
		
(define get-first-tag-of-seq
	(lambda (expr)
		(caar expr)))
		
(define swap-def-set
	(lambda (list-of-exprs)
		(cond ((null? list-of-exprs) list-of-exprs)
			  ((equal? (caar list-of-exprs) 'def) (cons `(set ,@(cdar list-of-exprs)) (swap-def-set (cdr list-of-exprs))))
			  (else (cons (car list-of-exprs) (swap-def-set (cdr list-of-exprs))))
		)
	)
)

(define make-def-list
	(lambda (lst)
		(cond ((null? lst) lst)
			  ((equal? (caar lst) 'def) (cons (cdar lst) (make-def-list (cdr lst))))
			  (else (make-def-list (cdr lst)))) ))
			  

(define make-false-helper
	(lambda (num acc)
		(cond ((eq? num 0) acc)
			  (else (cons `(const #f) (make-false-helper (- num 1) acc))))
		))

(define make-false 
	(lambda (num)
		(make-false-helper num '())
		))
				
(define make-letrec-from-seq
	(lambda (expr)
		(define var-vals (make-def-list expr))
		(define params (map cadar var-vals))
		(define body (swap-def-set expr))
		(define new-body (map eliminate-nested-defines body))
		`(applic 
			(lambda-simple 
				,params
				(seq ,new-body))
				,(make-false (length params)))
	)
)
		
(define do-elimination
	(lambda (expr)
		(cond ((eq? (car expr) 'seq) 
				(if (eq? (get-first-tag-of-seq (cadr expr)) 'def)
					(make-letrec-from-seq (cadr expr))
					expr))
			  ((eq? (car expr) 'def) (make-letrec-from-seq `(,expr)))
			  (else expr)
	    )		
	)				
)	
			
(define eliminate-nested-defines
	(lambda (expr)
		(if (null? expr) expr
			(let
				((tag (car expr))
				 (rest (cdr expr)))
			(cond ((eq? tag 'seq) `(,tag ,(map (lambda (tat-expr) (eliminate-nested-defines tat-expr)) (get-tatey-expr rest))))
				  ((eq? tag 'applic) `(,tag ,(eliminate-nested-defines (get-applic-operator rest)) 
									        ,(map (lambda (operand) (eliminate-nested-defines operand)) (get-applic-operands rest))))
				  ((eq? tag 'def) `(,tag ,(eliminate-nested-defines (get-define-var rest)) 
										 ,(eliminate-nested-defines (get-define-val rest))))
				  ((eq? tag 'if3) `(,tag ,(eliminate-nested-defines (get-if-condition rest)) 
										 ,(eliminate-nested-defines (get-if-first rest)) 
										 ,(eliminate-nested-defines (get-if-second rest))))
				  ((eq? tag 'or) `(,tag ,(map (lambda (tat-expr) (eliminate-nested-defines tat-expr)) (get-or-sub-exprs rest))))
				  ((eq? tag 'lambda-simple) `(,tag ,(get-lambda-parameters rest) ,(eliminate-nested-defines (do-elimination (get-lambda-body rest)))))
				  ((eq? tag 'lambda-opt) `(,tag ,(get-lambda-parameters rest) ,(get-lambda-opt-params rest) ,(eliminate-nested-defines (do-elimination (get-lambda-opt-body rest)))))
				  ((eq? tag 'lambda-var) `(,tag ,(get-lambda-parameters rest) ,(eliminate-nested-defines (do-elimination (get-lambda-body rest)))))
				  ((eq? tag 'set) `(,tag ,(eliminate-nested-defines (get-set-var rest)) ,(eliminate-nested-defines (get-set-val rest))))
				  (else expr)
				))
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define is-lambda-simple?
	(lambda (lambda-expr)
		(eq? (car lambda-expr) 'lambda-simple)
))

(define do-remove-nil
	(lambda (expr)
		(define operator (get-applic-operator (cdr expr)))
		(define operands (get-applic-operands (cdr expr)))
		(if (and (is-lambda-simple? operator) (eq? (length (get-lambda-parameters (cdr operator))) 0) (null? operands))
			(get-lambda-body (cdr operator))
			 expr)
	)
)

(define remove-applic-lambda-nil
	(lambda (expr)
		(if (null? expr) expr
			(let
				((tag (car expr))
				 (rest (cdr expr)))
			(cond ((eq? tag 'seq) `(,tag ,(map (lambda (tat-expr) (remove-applic-lambda-nil tat-expr)) (get-tatey-expr rest))))
				  ((eq? tag 'applic) (do-remove-nil `(,tag ,(remove-applic-lambda-nil (get-applic-operator rest))
														   ,(map remove-applic-lambda-nil (get-applic-operands rest)))))
				  ((eq? tag 'def) `(,tag ,(remove-applic-lambda-nil (get-define-var rest)) 
										 ,(remove-applic-lambda-nil (get-define-val rest))))
				  ((eq? tag 'if3) `(,tag ,(remove-applic-lambda-nil (get-if-condition rest)) 
										 ,(remove-applic-lambda-nil (get-if-first rest)) ,(remove-applic-lambda-nil (get-if-second rest))))
				  ((eq? tag 'or) `(,tag ,(map (lambda (tat-expr) (remove-applic-lambda-nil tat-expr)) (get-or-sub-exprs rest))))
				  ((eq? tag 'lambda-simple) `(,tag ,(get-lambda-parameters rest) 
												   ,(remove-applic-lambda-nil (get-lambda-body rest))))
				  ((eq? tag 'lambda-opt) `(,tag ,(get-lambda-parameters rest) 
												,(get-lambda-opt-params rest) 
												,(remove-applic-lambda-nil (get-lambda-opt-body rest))))
				  ((eq? tag 'lambda-var) `(,tag ,(get-lambda-parameters rest) 
											    ,(remove-applic-lambda-nil (get-lambda-body rest))))
				  ((eq? tag 'set) `(,tag ,(remove-applic-lambda-nil (get-set-var rest)) 
										 ,(remove-applic-lambda-nil (get-set-val rest))))
				  (else expr)
				))
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define v-not-in-params
	(lambda (v params)
                 (let ((new-params (if (not (list? params)) (list params) params)))
				 (if (null? new-params) #t
		(not (ormap (lambda (vi) (eq? v vi)) new-params))))
	))


(define is-bound
	(lambda (v body)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond
                                  ((eq? tag 'lambda-simple)
                                          (if (v-not-in-params v (get-lambda-parameters rest)) (has-get-bound v (get-lambda-body rest)) #f))
                                  ((eq? tag 'lambda-opt)
                                          (if (v-not-in-params v (append (get-lambda-parameters rest) (list (cadr rest)))) (has-get-bound v (caddr rest)) #f))
                                  ((eq? tag 'lambda-var) (if (v-not-in-params v (list (get-lambda-parameters rest))) (has-get-bound v (get-lambda-body rest)) #f))
				  ((eq? tag 'seq) (ormap (lambda (expr) (is-bound v expr)) (get-tatey-expr rest)))
				  ((eq? tag 'applic) (or (is-bound v (get-applic-operator rest))
                                                                (ormap (lambda (expr) (is-bound v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def) (or (is-bound v (get-define-var rest)) 
                                                             (is-bound v (get-define-val rest))))
				  ((eq? tag 'if3) (or (is-bound v (get-if-condition rest)) 
                                                             (is-bound v (get-if-first rest)) 
                                                             (is-bound v (get-if-second rest))))  
				  ((eq? tag 'or) (ormap (lambda (expr) (is-bound v expr)) (get-or-sub-exprs rest)))	  
				  ((eq? tag 'set) (is-bound v (get-set-val rest)))
                                  ((eq? tag 'box-set) (is-bound v (get-set-val rest)))
				  (else #f)
				))
                        )))


(define has-set
	(lambda (v body)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond ((eq? tag 'set) (if (eq? v (cadar rest))
                                                         #t
                                                         (has-set v (get-set-val rest))))
                              ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest)))
                                   (has-set v (get-lambda-body rest)))
				  ((eq? tag 'lambda-opt) (has-set v (get-lambda-opt-body rest)))
				  ((eq? tag 'seq) (ormap (lambda (expr) (has-set v expr)) (get-tatey-expr rest)))             
				  ((eq? tag 'applic) (or (has-set v (get-applic-operator rest))
                                                                (ormap (lambda (expr) (has-set v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def) (or (has-set v (get-define-var rest)) 
                                                             (has-set v (get-define-val rest))))
				  ((eq? tag 'if3) (or (has-set v (get-if-condition rest)) 
                                                             (has-set  v (get-if-first rest)) 
                                                             (has-set v (get-if-second rest))))
				  ((eq? tag 'or) (ormap (lambda (expr) (has-set v expr)) (get-or-sub-exprs rest)))
				  ((eq? tag 'set) (has-set v (get-set-val rest)))
                                  ((eq? tag 'box-set) (has-set v (get-set-val rest)))
				  (else #f)
				)))
		
	)
)

(define has-get
	(lambda (v body)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond ((eq? tag 'var) (eq? v (car rest)))
			      ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest))) (has-get v (get-lambda-body rest)))
				  ((eq? tag 'lambda-opt) (has-get v (get-lambda-opt-body rest)))
				  ((eq? tag 'seq) (ormap (lambda (expr) (has-get v expr)) (get-tatey-expr rest)))
				  ((eq? tag 'applic) (or (has-get v (get-applic-operator rest))
											(ormap (lambda (expr) (has-get v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def) (or (has-get v (get-define-var rest)) 
									  (has-get v (get-define-val rest))))
				  ((eq? tag 'if3) (or (has-get v (get-if-condition rest)) 
									  (has-get v (get-if-first rest)) 
									  (has-get v (get-if-second rest))))
				  ((eq? tag 'or) (ormap (lambda (expr) (has-get v expr)) (get-or-sub-exprs rest)))			  
				  ((eq? tag 'set) (has-get v (get-set-val rest)))
                                  ((eq? tag 'box-set) (has-get v (get-set-val rest)))
				  (else #f)
				))
		)
	)
)

(define has-get-bound
	(lambda (v body)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond ((eq? tag 'var) (eq? v (car rest)))
			      ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest)))
                               (has-get-bound v (get-lambda-body rest)))
				  ((eq? tag 'lambda-opt) (has-get-bound v (get-lambda-opt-body rest)))                         
				  ((eq? tag 'seq) (ormap (lambda (expr) (has-get-bound v expr)) (get-tatey-expr rest)))
				  ((eq? tag 'applic) (or (has-get-bound v (get-applic-operator rest))
                                                                (ormap (lambda (expr) (has-get-bound v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def) (or (has-get-bound v (get-define-var rest)) 
                                       (has-get-bound v (get-define-val rest))))
				  ((eq? tag 'if3) (or (has-get-bound v (get-if-condition rest)) 
                                       (has-get-bound v (get-if-first rest)) 
                                       (has-get-bound v (get-if-second rest))))            
				  ((eq? tag 'or) (ormap (lambda (expr) (has-get-bound v expr)) (get-or-sub-exprs rest)))
				  ((eq? tag 'set) (or (has-get-bound v (get-set-var rest)) (has-get-bound v (get-set-val rest))))
                                  ((eq? tag 'box-set)  (or (has-get-bound v (get-set-var rest)) (has-get-bound v (get-set-val rest))))
				  (else  #f)
				))
		)))

(define check-box-criteria 
	(lambda (parameter body)
                    (and (is-bound parameter body) (has-set parameter body) (has-get parameter body))
	))

 
(define add-first-set
	(lambda (v body)
		(if (eq? 'seq (car body))
			`(seq ((set (var ,v) (box (var ,v))) ,@(get-tatey-expr (cdr body))))
			`(seq ((set (var ,v) (box (var ,v))) ,(box-set body)))
		)))


(define replace-get
	(lambda (v body)
		(if (null? body) body
			(let ((tag (car body))
				 (rest (cdr body)))
                          (cond
                                  ((eq? tag 'var) 
                                                     (if (eq? v (car rest)) `(box-get ,body) body))
                                  ((and (eq? tag 'lambda-simple) (v-not-in-params v (get-lambda-parameters rest)))
						`(,tag ,(get-lambda-parameters rest) ,(replace-get v (get-lambda-body rest))))
                                  ((and (eq? tag 'lambda-var) (v-not-in-params v (list (get-lambda-parameters rest))))
						`(,tag ,(get-lambda-parameters rest) ,(replace-get v (get-lambda-body rest))))
				  ((eq? tag 'lambda-opt)  `(,tag ,(get-lambda-parameters rest) ,(get-lambda-opt-params rest) ,(replace-get v (get-lambda-opt-body rest))))
				  ((eq? tag 'seq)  `(,tag ,(map (lambda (expr) (replace-get v expr)) (get-tatey-expr rest))))
				  ((eq? tag 'applic) `(,tag ,(replace-get v (get-applic-operator rest)) ,(map (lambda (expr) (replace-get v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def)  `(,tag ,(replace-get v (get-define-var rest)) ,(replace-get v (get-define-val rest))))
				  ((eq? tag 'if3)  `(,tag ,(replace-get v (get-if-condition rest)) ,(replace-get v (get-if-first rest)) ,(replace-get v (get-if-second rest))))
				  ((eq? tag 'or)`(,tag ,(map (lambda (expr) (replace-get v expr)) (get-or-sub-exprs rest))))
				  ((eq? tag 'set)  `(,tag ,(replace-get v (get-set-var rest)) ,(replace-get v (get-set-val rest))))
                                  ((eq? tag 'box-set) `(,tag ,(get-set-var rest) ,(replace-get v (get-set-val rest))))
				  (else body)
				))
		)))

(define replace-set
	(lambda (v body)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond
                                  ((eq? tag 'set) (if (eq? v (cadar rest))
                                                             `(box-set ,(get-set-var rest) ,(replace-set v (get-set-val rest)))
                                                             `(set ,(get-set-var rest) ,(replace-set v (get-set-val rest)))))
                                  ((eq? tag 'box-set) `(box-set ,(get-set-var rest) ,(replace-set v (get-set-val rest))))
                                  ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest)))
                                                         `(,tag ,(get-lambda-parameters rest) ,(replace-set v (get-lambda-body rest))))
				  ((eq? tag 'lambda-opt) `(,tag ,(get-lambda-parameters rest) 
                                                                ,(get-lambda-opt-params rest) 
                                                                ,(replace-set v (get-lambda-opt-body rest))))
				  ((eq? tag 'seq) `(,tag ,(map (lambda (expr) (replace-set v expr)) (get-tatey-expr rest))))
				  ((eq? tag 'applic) `(,tag ,(replace-set v (get-applic-operator rest))
                                                            ,(map (lambda (expr) (replace-set v expr)) (get-applic-operands rest))))
				  ((eq? tag 'def) `(,tag ,(replace-set v (get-define-var rest)) 
                                                         ,(replace-set v (get-define-val rest))))
				  ((eq? tag 'if3) `(,tag ,(replace-set v (get-if-condition rest)) 
                                                         ,(replace-set v (get-if-first rest))
                                                         ,(replace-set v (get-if-second rest))))
				  ((eq? tag 'or) `(,tag ,(map (lambda (expr) (replace-set v expr)) (get-or-sub-exprs rest))))
				  (else  body)
                                  )))))


(define let-us-box
	(lambda (v body)
                 (box-set (add-first-set v (box-set (replace-get v (box-set (replace-set v body))))))
                 ))
  

(define do-boxing
  (lambda (params body)
          (cond ((null? params) (box-set body)) 
                (else
                 (let ((v (car params)))
                          (if (check-box-criteria v body)
                              (do-boxing (cdr params) (let-us-box v body))
                              (do-boxing (cdr params) body)
                              ))))))

(define box-set 
	(lambda (expr)
		(if (null? expr) expr
			(let
				((tag (car expr))
				 (rest (cdr expr)))

			(cond
                              
                              ((eq? tag 'seq) `(,tag ,(map (lambda (tat-expr) (box-set tat-expr)) (get-tatey-expr rest)))) 
                              ((eq? tag 'applic)  `(,tag ,(box-set (get-applic-operator rest))
                                                         ,(map (lambda (operand) (box-set operand)) (get-applic-operands rest))))              
                              ((eq? tag 'def) `(,tag ,(box-set (get-define-var rest)) 
                                                     ,(box-set (get-define-val rest))))
                              ((eq? tag 'if3) `(,tag ,(box-set (get-if-condition rest)) 
                                                     ,(box-set (get-if-first rest)) 
                                                     ,(box-set (get-if-second rest))))
                              ((eq? tag 'or) `(,tag ,(map (lambda (tat-expr) (box-set tat-expr)) (get-or-sub-exprs rest))))
                              ((eq? tag 'lambda-simple) `(,tag ,(get-lambda-parameters rest) ,(do-boxing (list-reverse (get-lambda-parameters rest)) (get-lambda-body rest))))
                              ((eq? tag 'lambda-opt) `(,tag ,(get-lambda-parameters rest)
                                                            ,(get-lambda-opt-params rest)
                                                            ,(do-boxing (list-reverse (get-lambda-parameters rest)) (get-lambda-opt-body rest))))
                              ((eq? tag 'lambda-var) `(,tag ,(get-lambda-parameters rest) ,(do-boxing (add-sograim (get-lambda-parameters rest)) (get-lambda-body rest))))
                              ((or (eq? tag 'set) (eq? tag 'box-set)) `(,tag ,(get-set-var rest) ,(box-set (get-set-val rest))))
                              (else  expr)
				)))
		))	

;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;TASK 6 -START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-lambda-params
  (lambda (exp)
    (cond
      ((equal? (car exp) 'lambda-simple) (cadr exp))
      ((equal? (car exp) 'lambda-opt) (append (cadr exp) (list (caddr exp))))
      ((equal? (car exp) 'lambda-var) (cadr exp))
    )))

(define add-to-list
  (lambda (lst what-to-add)
    (append what-to-add lst)
 ))

(define member?
    (lambda (elem lst)
      (cond ((equal? lst '()) #f)
            ((equal? (car lst) elem) (car lst))
            (else (member? elem (cdr lst))))
      ))

(define member-first?
  (lambda (elem lst)
    (cond ((equal? lst `()) #f)
          ((and (list? (car lst)) (equal? (car (car lst)) elem)) (car lst))
          (else (member-first? elem (cdr lst))))))

      
(define stick-together
  (lambda (a b)
    (cond ((or (equal? a `()) (equal? b `())) (append a b))
          ((and (list? a) (list? b)) (list a b))
          ((and (list? a) (not (list? b)))  (append a (list b)))
          ((and (not (list? a)) (list? b))  (cons a b))
          ((and (not (list? a)) (not (list? b))) (list a b)))))
          


(define tagged-by?
  (lambda (exp tag)
    (and (list? exp) (equal? (car exp) tag))
   ))

;;var? predicate of part3
(define var-3?
  (lambda (exp)
    (tagged-by? exp 'var)))

(define special-var?
  (lambda (exp)
    (or (tagged-by? exp 'pvar) (tagged-by? exp 'bvar) (tagged-by? exp 'fvar))))

;;lambda? predicate of part3
(define lambda-3?
  (lambda (exp)
    (or (tagged-by? exp 'lambda-simple) (tagged-by? exp 'lambda-var) (tagged-by? exp 'lambda-opt))))

;;l-exp is a lambda expression, meaning (lambda-3? l-exp) ==> #t
(define get-body-3
  (lambda (l-exp)
    (cond ((equal? (car l-exp) 'lambda-simple) (caddr l-exp))
          ((equal? (car l-exp) 'lambda-opt) (cadddr l-exp))
          ((equal? (car l-exp) 'lambda-var) (caddr l-exp))
    
    )))

(define get-header-3
  (lambda (l-exp)
    (list (car l-exp) (cadr l-exp))
    ))

;; vars-lst must be a list whose every element is a list of 3 elements
(define make-lexical-var
  (lambda (var-record vars-lst current-major)
    (let ((var-name (cadr var-record)))
    (cond ((equal? (member-first? var-name vars-lst) #f) `(fvar ,var-name) ) 
          (else
           (let* ((var-record (member-first? var-name vars-lst))
                    (var-major (cadr var-record))
                    (var-minor (caddr var-record))
                    (major-diff (- current-major var-major)))
             (if (equal? major-diff 0)
                 `(pvar ,var-name ,var-minor) 
                 `(bvar ,var-name ,(- major-diff 1) ,var-minor) 
             )))
          ))
    ;)
    
    ))

(define element-index
  (lambda (item lst)
      (- (length lst) (length (memv item lst)) )   
    ))

(define update-vars-lst
  (lambda (vars-lst lambda-params current-major)
    (let* ((new-lambda-params (if (not (list? lambda-params)) (list lambda-params) lambda-params))
           (new-vars-lst  (remove-elements-from-lst new-lambda-params vars-lst)))
      (append new-vars-lst (map (lambda (item) (list item current-major (element-index item new-lambda-params))) new-lambda-params)))

    ))

(define remove-elements-from-lst
 (lambda (lst-items-to-delete lst)
   (map (lambda (item) (set! lst (remove-element-from-lst item lst))) lst-items-to-delete)
   lst
   ))


;;pre-condition : lst is a list of lists! lst='((a b c) (c d e))
;; removes elements that start with the item..
;;removes all sublists that start with item-to-delete
(define remove-element-from-lst
  (lambda (item-to-delete lst)
    (cond ((equal? lst '()) '())
          ((equal? (caar lst) item-to-delete) (remove-element-from-lst item-to-delete (cdr lst)))
          (else (cons (car lst) (remove-element-from-lst item-to-delete (cdr lst))))
    )))

(define annotate-lexical
  (lambda (exp vars-lst current-major)
         (cond
            ((or (not (list? exp)) (equal? exp '()))  exp)
            
            ((lambda-3? exp) 
             (let ((new-vars-lst (update-vars-lst vars-lst (get-lambda-params exp) (+ current-major 1))))
                               (cond
                                 ((equal? (car exp) 'lambda-opt)
                                   (cons (car exp) (list (cadr exp) (caddr exp) (annotate-lexical (get-body-3 exp) new-vars-lst (+ current-major 1)))))
                                 (else
                                   (cons (car exp) (list (get-lambda-params exp) (annotate-lexical (get-body-3 exp) new-vars-lst (+ current-major 1))))))))
                                 
                                          
            ((equal? (car exp)'var) 
                     (make-lexical-var exp vars-lst current-major))
            (else 
             (cons (annotate-lexical (car exp) vars-lst current-major) (annotate-lexical (cdr exp) vars-lst current-major)))
            )))
                             

(define pe->lex-pe
  (lambda (expr)
    (annotate-lexical expr '() -1)
    ))




(define const-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'const))
    ))

(define var-p3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'var))
    ))

(define or-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'or))
    ))

(define seq-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'seq))
    ))

(define if-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'if3))
    ))

  
;;must be applied on an expression of type: (or lst) where lst can be '()
(define get-or-first-element-3
  (lambda (exp)
    (if (equal? (cdr exp) '())
        '()
        (cadr exp))
    ))

(define define-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'def))
    ))

(define applic-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'applic))
    ))

(define set-3?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'set))
    ))

(define box-set?
  (lambda (exp)
    (and (not (equal? exp '())) (equal? (car exp) 'box-set?))))

;;doesn't deal with lambda-opt
(define get-lambda-params-7
  (lambda (exp)
    (cond
      ((or (equal? (car exp) 'lambda-simple) (equal? (car exp) 'lambda-var)) (cadr exp))
     
    )))

(define get-all-but-last-elem
  (lambda (exp)
    (if (equal? (cdr exp) '())
        '()
        (cons (car exp) (get-all-but-last-elem (cdr exp))))))

(define get-last-elem
  (lambda (exp)
    (if (equal? (cdr exp) '())
        (car exp)
         (get-last-elem (cdr exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 - END ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 - START ;;;;;;;;;;;;;;;;;;;;;



(define tc-annotating
  (lambda (expr tp?)
    (cond
      ((equal? expr '())  '())
      ((or (const-3? expr) (var-p3? expr) (special-var? expr))  expr)
      ((or-3? expr)  `(or ,(append (map (lambda (item) (tc-annotating item #f))  (get-all-but-last-elem (cadr expr)))  (list (tc-annotating (get-last-elem (cadr expr)) tp?)) )))
      ((seq-3? expr) `(seq ,(append (map (lambda (item) (tc-annotating item #f))  (get-all-but-last-elem (cadr expr)))  (list (tc-annotating (get-last-elem (cadr expr)) tp?)) )))
      ((if-3? expr) `( ,(car expr) ,(tc-annotating (cadr expr) #f) ,(tc-annotating (caddr expr) tp?) ,(tc-annotating (cadddr expr) tp?)))
      ((define-3? expr) `(def ,(cadr expr) ,(tc-annotating (caddr expr) #f)))
      ((set-3? expr)  `(set ,(cadr expr) ,(tc-annotating (caddr expr) #f)))
      ((lambda-3? expr) (if (equal? (car expr) 'lambda-opt)
                                   `(,(car expr) ,(cadr expr) ,(caddr expr) ,(tc-annotating (get-body-3 expr) #t))
                                   `(,(car expr) ,(get-lambda-params-7 expr) ,(tc-annotating (get-body-3 expr) #t))))
      ((applic-3? expr) (if (equal? tp? #t)
                                 `(tc-applic ,(tc-annotating (cadr expr) #f) ,(map (lambda (item) (tc-annotating item #f)) (caddr expr)))
                                 `(applic ,(tc-annotating (cadr expr) #f) ,(map (lambda (item) (tc-annotating item #f)) (caddr expr)))))
      ((box-set? expr) `(box-set ,(cadr expr) ,(tc-annotating (caddr expr) #f)))
      (else (map (lambda (item) (if (list? item) (tc-annotating item #f) item)) expr))
      )))



(define annotate-tc
  (lambda (expr)
    (tc-annotating expr #f)
    ))



;;;;;;;;;;;;;;;;;;; TASK 7 -END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;; CGEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;; helper-functions ;;;;

(define lst-without-last-elem
  (lambda (exp)
    (let ((start (car exp))
          (rest (cdr exp)))
      (if (equal? rest '())
          rest
          (cons (car exp) (lst-without-last-elem (cdr exp))))
    )))

(define last-elem-of-lst
  (lambda (exp)
    (let ((start (car exp))
          (rest (cdr exp)))
      (if (equal? rest '())
                  start
                  (last-elem-of-lst rest)))))


;;;;;;; end helper-functions ;;;

(define T_VOID 		937610)
(define T_NIL 		722689)
(define T_BOOL 		741553)
(define T_CHAR 		181048)
(define T_INTEGER 	945311)
(define T_STRING 	799345)
(define T_SYMBOL 	368031)
(define T_PAIR 		885397)
(define T_VECTOR 	335728)
(define T_CLOSURE 	276405)

;; Free addresses
 ;; 451794 544512 183403 101555 957412 645713 110463 358902
 ;; 511179 615181 602995 921183 685967 286949 743140 529751 
 ;; 569917 510364 183731 805664 111363 248183 911091 400183
 ;; 922870 152305 777096 155936 721327 775216 722858 864017
 ;; 508854 414227 572874 294755 989204 646495 725032 234097


;;;;;;;;;;;;;;;; CONST TABLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Define the lists/table:

(define const-lst '())

(define sub-exps-lst '())

(define const-table
  '((10 `,(void) (T_VOID))
    (11 () (T_NIL))
    (12 #f (T_BOOL O))
    (14 #t (T_BOOL 1))))

(define next-free-start-address 16)

(define void?
  (lambda (exp)
    (equal? exp `,(void))
    ))

(define update-const-lst
 (lambda (new-item)
   (set! const-lst (cons new-item const-lst))
   ))

  
(define update-sub-exps-lst
  (lambda (new-item)
    (set! sub-exps-lst (cons new-item sub-exps-lst))))

;; pre-condition: exp is a list
(define is-contains-lst?
  (lambda (exp)
    (ormap list? exp)
    ))


(define go-over-const-exp
  (lambda (exp)
    (cond ((equal? exp '()) '())
          ((not (list? exp)) exp)
          (else ;; this is a non empty list!
           (if (equal? (car exp) 'const)
               (begin (display "found const:") (display (cdr exp)) (display "\n") (update-const-lst (cdr exp))
                      (cons (go-over-const-exp (car exp)) (go-over-const-exp (cdr exp))))
               (cons (go-over-const-exp (car exp)) (go-over-const-exp (cdr exp))))
          ))))


(define topological-sort
  (lambda (lst)
      (let* ((res (begin (extract-sub-exps lst) (remove-dups sub-exps-lst)))
             (only-non-list-sub-exps (filter (lambda (item) (not (list? item))) res))
             (only-list-sub-exps (filter (lambda (item) (list? item)) res)))
        (append only-non-list-sub-exps only-list-sub-exps) )
    ))


(define extract-sub-exps
  (lambda (lst)
    (cond ((equal? lst '())  '())
          ((vector? lst) (begin (map update-sub-exps-lst (vector->list lst)) lst)) ;;not sure if this is necessary???
          ((symbol? lst) (begin (update-sub-exps-lst lst) (update-sub-exps-lst (symbol->string lst)) lst))
          ((not (list? lst)) (update-sub-exps-lst lst) lst)
          (else (begin (update-sub-exps-lst lst) 
                       (update-sub-exps-lst (car lst)) (update-sub-exps-lst (cdr lst)) 
                       (cons (extract-sub-exps (car lst)) (extract-sub-exps (cdr lst)))))
          )))

     
(define remove-dups
  (lambda (lst)
    (if (equal? lst '())
        '()
        (let ((first (car lst)))
          (cons first (remove-dups (filter (lambda (item) (not (equal? item first))) lst))))
        )))

(define collect-constants-cycle
  (lambda (exp)
    (begin (go-over-const-exp exp) ;we now have const-lst that includes all the constants
           (set! const-lst (remove-dups const-lst))
           (set! const-lst (topological-sort const-lst)) ;; we now have a topologicaly sorted const-lst
           (make-const-table const-lst) ;; turn the list into the table with the required structure (+ #f #t void null)
           ;(load-const-table-to-memory const-table)
    )))

;;search for the item in the const-table! (not const-lst)
(define lookup-const-table
  (lambda (item-to-find const-tbl-local)
    (cond ((equal? const-tbl-local '()) '()) ;meaning the record wasn't found
          ((equal? (cadar const-tbl-local) item-to-find) (caar const-tbl-local))
          (else (lookup-const-table item-to-find (cdr const-tbl-local))))
    ))

;;PROBABLY NOT CORRECT!!
  (define fraction?
    (lambda (exp)
      (and (equal? (integer? exp) #f) (equal? (number? exp) #t))
      ))

(define make-const-table
  (lambda (lst)
    ;; i have a const-lst! and a const-table
    ;; i want to remove items from the const-lst and add them to const-table after doing (make-new-record) on them!
     ;(if (equal? lst '())
     ;    '()
     ;    (begin (set! const-table (append const-table (list (make-new-record (car lst))))) (set! const-lst (cdr const-lst))
     ;           (make-const-table const-lst)))
    (cond ((equal? lst '()) '())
          ((or (equal? (car lst) `,(void)) (equal? (car lst) '()) (equal? (car lst) #t) (equal? (car lst) #f) )
           (begin (set! const-lst (cdr const-lst)) (make-const-table const-lst)))
          (else (begin (set! const-table (append const-table (list (make-new-record (car lst))))) (set! const-lst (cdr const-lst))
                (make-const-table const-lst))))
    ))

(define make-new-record
  (lambda (exp)
    (let ((old-next-start-address next-free-start-address))
      (cond ((void? exp) (begin (set! next-free-start-address (+ 1 old-next-start-address))
                        `(,old-next-start-address ,exp ,T_VOID)))
            ((null? exp) (begin (set! next-free-start-address (+ 1 old-next-start-address))
                        `(,old-next-start-address ,exp ,T_NIL)))
            ((boolean? exp) (begin (set! next-free-start-address (+ 2 old-next-start-address))
                                   (if (equal? exp #t)
                                     `(,old-next-start-address ,exp (,T_BOOL 0))
                                     `(,old-next-start-address ,exp (,T_BOOL 1)))))
            ((number? exp) (begin (set! next-free-start-address (+ 2 old-next-start-address))
                           `(,old-next-start-address ,exp (,T_INTEGER ,exp))))
            ;((fraction? exp) `(,next-start-address ,exp (T_frac ??)))
            ((pair? exp) (begin (set! next-free-start-address (+ 3 old-next-start-address))
                                (let ((first-elem (lookup-const-table (car exp) const-table))
                                      (second-elem (lookup-const-table (cdr exp) const-table)))
                                  `(,old-next-start-address ,exp (,T_PAIR ,first-elem ,second-elem)))))
            ((vector? exp) (let* ((vector-body (vector->list exp))
                                  (body-address (map (lambda (item) (lookup-const-table item const-table)) vector-body)))
                             (begin (set! next-free-start-address (+ 2 (vector-length exp) old-next-start-address))
                             `(,old-next-start-address ,exp (,T_VECTOR ,(vector-length exp) ,@body-address) ))))
            ((string? exp) (let* ((string-lst (string->list exp))
                                  (string-lst-unicode (map char->integer string-lst)))
                             (begin (set! next-free-start-address (+ 2 (length string-lst) old-next-start-address))
                              `(,old-next-start-address ,exp (,T_STRING ,(length string-lst) ,@string-lst-unicode)))))
            ;((closure? exp) ...)
            ((char? exp) (begin (set! next-free-start-address (+ 2 old-next-start-address))
                                `(,old-next-start-address ,exp (,T_CHAR ,exp))))
            ((symbol? exp) (begin (set! next-free-start-address (+ 2 old-next-start-address))
                                   `(,old-next-start-address ,exp (T_SYMBOL ,(lookup-const-table (symbol->string exp) const-table)))) ))
      )))


(define make-const-lst-for-mem
  (lambda (tbl)
    (

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;free vars table;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fvar-lst '())

(define update-fvar-lst
 (lambda (new-item)
   (set! fvar-lst (cons new-item fvar-lst))
   ))

(define go-over-fvar-exp
  (lambda (exp)
    (cond ((equal? exp '()) '())
          ((not (list? exp)) exp)
          (else ;; this is a non empty list!
           (if (equal? (car exp) 'fvar)
               (begin (display "found fvar:") (display (cdr exp)) (display "\n") (update-fvar-lst (cdr exp))
                      (cons (go-over-fvar-exp (car exp)) (go-over-fvar-exp (cdr exp))))
               (cons (go-over-fvar-exp (car exp)) (go-over-fvar-exp (cdr exp))))
          ))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Symbol-list;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define full-cycle
  (lambda (input)
    (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines input)))))
    ))

(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name
                       (number->string n))))))

;;label list : 
(define ^label-if3else (^^label "Lif3else"))
(define ^label-if3exit (^^label "Lif3exit"))
(define ^label-end-program (^^label "Lend"))
(define label-end (^label-end-program))
(define nl (list->string (list #\newline)))
(define tab "\t")

(define prologue "/* cisc.c

#include <stdio.h>
#include <stdlib.h>

/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1
#define T_VOID 		937610
#define T_NIL 		722689
#define T_BOOL 		741553
#define T_CHAR 		181048
#define T_INTEGER 	945311
#define T_STRING 	799345
#define T_SYMBOL 	368031
#define T_PAIR 		885397
#define T_VECTOR 	335728
#define T_CLOSURE 	276405
#include \"cisc.h\"

int main()
{
  START_MACHINE;

  JUMP(CONTINUE);

#include \"char.lib\"
#include \"io.lib\"
#include \"math.lib\"
#include \"string.lib\"
#include \"system.lib\"

 CONTINUE:")

(define epilogue
  (let ();((label-end (^label-end-program)))
  (string-append
   tab "CMP (R0, IMM(T_NIL));" nl
   tab "JUMP_EQ("label-end");" nl
   tab "SHOW (\"STR[0] = \", R0);" nl
   label-end ":" nl
   tab "STOP_MACHINE;" nl
   tab "return 0;" nl
   "}"
   )))

(define *left-to-implement* "
append (variadic), apply, < (variadic), = (variadic), > (variadic), + (variadic), / (variadic),
* (variadic), - (variadic), boolean?, car, cdr, char->integer, char?, cons, denominator,
eq?, integer?, integer->char, list (variadic), make-string, make-vector, map, not, null?,
number?, numerator, pair?, procedure?, rational?, remainder, set-car!, set-cdr!, string-length,
string-ref, string-set!, string->symbol, string?, symbol?, symbol->string, vector, vector-length,
vector-ref, vector-set!, vector?, zero?")

(define listOfExps
  (lambda (parser string)
    (parser (string->list string)
	    (lambda (e s)
	      (append (list e)
		(listOfExps <sexpr> (list->string s))))
	    (lambda (w) `()))))


(define code-gen-const
  (lambda (exp)
    (let ((const-address (lookup-const-table exp const-table)))
    (string-append tab "MOV (R0,IMM("const-address");" nl))
    ))

  
(define code-gen-if3
          (lambda (test do-if-true do-if-false)
            (let ((code-test "1")
                  (code-dit "2")
                  (code-dif "3")
                  (label-else (^label-if3else))
                  (label-exit (^label-if3exit)))
              (string-append
               nl
               tab code-test nl ; when run, the result of the test will be in R0
               tab "CMP(R0, IMM(0));" nl
               tab "JUMP_EQ(" label-else ");" nl
               tab code-dit nl
               tab "JUMP(" label-exit ");" nl
               label-else ":" nl
               tab code-dif nl
               label-exit ":" nl))))

(define code-gen-or
  (lambda (args)
    ;(display "inside c-gen-or") (display "\n")
    ;(display args)
    ;; CMP (R0, IMM(0));
    ;; JUMP_EQ(Lend1);
    (let ;((args-after-code-gen (map code-gen args))
          ((args-after-code-gen '("a" "b" "c" "d" "e")))
      (string-append (fold-right
                      (lambda (item1 item2)
                        (string-append  item1 nl "CMP (R0, IMM(0));" nl "JUMP_EQ("label-end")" nl item2))
                        ""
                        (lst-without-last-elem args-after-code-gen))
                      (last-elem-of-lst args-after-code-gen)))
    ;(map (lambda (item)
    ;       (if 
    ;       (string-append (code-gen item) ))
    ;     args)
   ))

;;pre-condition: exp is cadr of original expression (doesn't include tag)
(define prepare-for-cgen-or
  (lambda (exp)
    (display "hi inside prepare-c-gen-or") (display "\n")
    (code-gen-or (car exp))
    ))

;; pre-condition: exp is cdr of original expression (doesn't include tag)
(define prepare-for-cgen-if3
  (lambda (exp)
    ;(display (get-if-condition exp)) (display (get-if-first exp)) (display (get-if-second exp))
    ;(display "newline")
    (code-gen-if3 (get-if-condition exp) (get-if-first exp) (get-if-second exp))
    ;; sends to code-get-if-3
  ))
  
(define code-gen
  (lambda (exp)
    (let ((tag (car exp))
          (rest (cdr exp)))
      (cond
        ((equal? tag 'if3) (prepare-for-cgen-if3 rest))
        ((equal? tag 'or) ;(display "identified or") (display "\n")
         (prepare-for-cgen-or rest))
        (else  exp)
        )
      )))

(define compile-scheme-file
  (lambda (scmFile cTrgFile)
    (let ((out-port (open-output-file (symbol->string cTrgFile) 'truncate)))
      ;;;should be replaced with 
       (display (string-append 
                prologue
               (let ((c-gen-res 
                                (map code-gen
                                     (map full-cycle
                                         (map parse 
                                               (listOfExps <sexpr> (file->string scmFile)))))))
                  ;(display c-gen-res) (display "\n")
                  (cond ((list? c-gen-res) (car c-gen-res) )
                        (else c-gen-res)))
                
                epilogue)
       out-port)
      (close-output-port out-port))))