(load "pc.scm")
(load "pattern-matcher.scm")
 
;(define <Boolean> <fail>)
;(define <
;(define <Number> <fail>)
;(define <String> <fail>)
;(define <Symbol> <fail>)
;(define <ProperList> <fail>)
;(define <ImproperList> <fail>)
;(define <Vector> <fail>)
;(define <Quoted> <fail>)
;(define <QuasiQuoted> <fail>)
;(define <Unquoted> <fail>)
;(define <UnquoteAndSpliced> <fail>)
(define <InfixExtension> <fail>)

(define <Boolean>
  (new (*parser (word-ci "#f"))
       (*pack (lambda (_) #f))
       (*parser (word-ci "#t"))
       (*pack (lambda (_) #t))
       
       (*disj 2)
       done))
       
(define <hex-char>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))
	 
         
(define <CharPrefix>
  (new (*parser (word "#\\"))
       done
       ))         

(define hexa->digit 
        (lambda (lst)
           (if (null? lst) 0
               (+ (* (car lst) (expt 16 (sub1 (length lst))))  (hexa->digit(cdr lst)))
        )))
       

(define <VisibleSimpleChar>(range #\! #\~))

;(define <VisibleSimpleChar>
;       (new (*parser <VisibleSimpleChar-1>)
;            (*pack (lambda (a) (string->symbol (string a))))
;            done
;            ))
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
	 
(define <HexUnicodeChar> 
  (new (*parser (char-ci #\x))
       (*parser <hex-char>) *plus
       (*caten 2)
       (*pack-with (lambda (_ rest)
	   (if (and (>= (hexa->digit rest) 0) (< (hexa->digit rest)  1114112))
            (integer->char (hexa->digit rest))
			'failed-to-convert)))
       done))

       
(define <NamedChar>
(new   ;(*parser (^<meta-char> "\\n" #\n))
       (*parser (word-ci "lambda"))
       (*pack (lambda (_) (integer->char 955)))
       (*parser (word-ci "newline"))
       (*pack (lambda (_) #\newline))
       (*parser (word-ci "nul"))
       (*pack (lambda (_) #\nul)) 
       (*parser (word-ci "page"))
       (*pack (lambda (_) #\page))
       (*parser (word-ci "return"))
       (*pack (lambda (_) #\return))
       (*parser (word-ci "space"))
       (*pack (lambda (_) #\space))
       (*parser (word-ci "tab"))
       (*pack (lambda (_) #\tab))
       (*disj 7)
       done))
	    

(define <Char> 
  (new (*parser <CharPrefix>)
       (*parser <HexUnicodeChar>)
	   (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       
       (*disj 3)
	   (*guard (lambda(e) (not (eq? e 'failed-to-convert))))
       (*caten 2)
       (*pack-with (lambda (first rest) rest))
       done)
       )
       
(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <Natural>
  (new (*parser <digit-0-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  s))

       
       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (* -1 s)))

       (*parser <Natural>)

       (*disj 3)

       done))
       

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
	(lambda (a b c)
	  (/ a c)))
       done))

       
 (define <digit-a-z>
  (range #\a #\z))
  
(define <digit-A-Z>
  (range #\A #\Z))
  
  
(define <Number>
      (new (*parser <Fraction>)
           (*parser <Integer> )
           (*parser <digit-a-z>)
           (*parser <digit-A-Z>)
           (*disj 2)
           *not-followed-by
           (*disj 2)done))
 

  
(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser <digit-a-z>)
       (*parser <digit-A-Z>)
	   (*pack (lambda (a) (char-downcase  a)))
       
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
	   ;*pack (lambda (a) (list->string a)))
	   (*pack (lambda (a) a))
       done))
       
(define <Symbol>
   (new (*parser <SymbolChar>)*plus
            (*pack (lambda (a) (string->symbol (list->string a))))
            done
            ))


	
(define <StringLiteralChar> 
(new (*parser <any-char>)
     (*parser (char #\\))
     (*parser (char #\"))
     (*disj 2)
     *diff
     ;(*pack (lambda (a) (string->symbol (string a))))
     (*pack (lambda (a) a))
     done))


	 
(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*disj 6)
       done))           

(define <StringHexChar> 
  (new (*parser (word-ci "\\x"))
       (*parser <hex-char>) *star
	   (*guard (lambda(rest)
				(and (>= (hexa->digit rest) 0) (< (hexa->digit rest)  1114112))))
       (*parser (char #\;))
       (*caten 3)
       (*pack-with (lambda (first mid rest) (integer->char (hexa->digit mid))))
  done))


	 
(define <StringChar> 
 (new (*parser <StringHexChar>)
      (*parser <StringLiteralChar>)
      (*parser <StringMetaChar>)
      (*disj 3)
      done))

(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>)*star
       (*parser (char #\"))
       (*caten 3)
       (*pack-with (lambda (first mid rest) (list->string mid)))
      done))
  
;(define <String>
;  (new (*parser (char #\"))
 ;      (*parser <StringChar>)*star
 ;      (*parser (char #\"))
 ;      (*caten 3)
 ;      (*pack-with (lambda (first mid rest)  mid))
 ;     done))
 
(define <Whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))	 
	 

       

(define <Mywhitespace> (range (integer->char 0) (integer->char 32)))


(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       	   (*delayed (lambda () <InfixExpression>))

	   (*delayed (lambda () <sexpr>))
	   (*disj 2)
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <Skip>
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

(define ^<skipped*> (^^<wrapped> (star <Skip>)))


(define add-list
  (lambda (s)
    (fold-right
     (lambda (a b) (+ a b))
     0
     s)))

;;;




       

	
	
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      
(define <ProperList>
(new (*parser (char #\())
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))*star
     (*parser (char #\)))
     (*caten 3)
     (*pack-with
	 (lambda (a b c)
       b ))
       done))
       
(define <ImproperList>
(new (*parser (char #\())
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>) )
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda ( a b c) b))*plus
     (*parser (char #\.))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>) )
	 (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda ( a b c ) b))
     (*parser (char #\)))
     (*caten 5)
     (*pack-with (lambda (a b c d e ) `(,@b . ,d)))
     done))
     
     
(define <Vector>
(new (*parser (word-ci "#("))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>) )
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c ) b ))*star
     (*parser (char #\)))
     (*caten 3)
     (*pack-with (lambda (a b c)  (list->vector b)))
     done))
     
     
(define <Quoted>
(new (*parser (char #\'))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
	 (*pack-with (lambda (a b) (list 'quote b)))
;     (*pack-with (lambda (a b) `',b))
     done))
     

(define <QuasiQuoted>
(new (*parser (char #\`))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
     (*pack-with (lambda (a b) (list 'quasiquote  b)))
     done))
 
(define <UnquoteAndSpliced>
(new (*parser (word-ci ",@"))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
     (*pack-with (lambda (a b) (list 'unquote-splicing b)))
     done))
 
(define <Unquoted>
(new (*parser (char #\,))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
     (*pack-with (lambda (a b) (list 'unquote b)))
     done))

;////////////////INFIX//////////////////

	
(define <InfixSymbol>
(new (*parser <SymbolChar>)
     ;(*pack-with (lambda (a) (char-downcase a)))
     (*parser (char #\+))
     (*parser (char #\-))
	 (*parser (char #\*))
     (*parser (char #\^))
	 (*parser (word-ci "**"))
     (*parser (char #\/))
	 (*parser (char #\())
	 (*parser (char #\)))
	 (*parser (char #\]))
	 (*parser (char #\[))
	 (*parser (char #\,))
	 (*disj 11)
	 *diff
	 *plus
	(*pack (lambda (a) (string->symbol (list->string  a))))
done))

	 
(define <Atom>
(new (*parser <Skip>)*star
     (*parser <Number>)
	 (*delayed (lambda () <InfixParen>))
	 (*parser <InfixSymbol>)
	 (*disj 3)
	 (*parser <Skip>)*star
	 (*caten 3)
	 (*pack-with (lambda (a b c ) b))
	 (*delayed (lambda ()<InfixArrayGet>))
	 (*delayed (lambda () <InfixFuncall>))
		
	 (*disj 2)
	 *star
	 (*caten 2)
	 (*pack-with (lambda (a lambda_lst) 
		(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
	 
	 done))
	 
(define <PowerSymbol>
(new (*parser (char #\^))
     (*parser (word-ci "**"))
	 (*disj 2)
	 done))
	 



;(define <InfixAdd> <fail>)
;(define <InfixNeg> <fail>)
;(define <InfixSub> <fail>)
;(define <InfixMul> <fail>)
;(define <InfixDiv> <fail>)
;(define <InfixPow> <fail>)
;(define <InfixArrayGet> <fail>)
;(define <InfixFuncall> <fail>)
;(define <InfixParen> <fail>)
;(define <InfixSexprEscape> <fail>)
;(define <InfixSymbol> <fail>)
;(define <Number> <fail>)	 
	 


(define <InfixNeg>
(new (*parser (char #\-))
     (*parser <Skip>)*star
	 (*parser <Atom>)
	 
     (*delayed (lambda () <InfixExpression>))
	 (*disj 2)
	 (*parser <Skip>)*star
	 (*caten 3)
	 (*pack-with (lambda (a b c ) b))
	 (*caten 2)
	 (*pack-with (lambda (a b)  (list (string->symbol(string a)) b)))
done))

(define <InfixAddandSub>
(new (*parser <Skip>) *star
     (*delayed (lambda () <InfixMulandDiv>))
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
     (*parser (char #\+))
	 (*parser (char #\-))
	 (*disj 2)
	 (*parser <Skip>) *star
	 (*delayed (lambda () <InfixMulandDiv>))
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
	 (*caten 2)
	 (*pack-with (lambda (b c) (lambda (a) (list (string->symbol (string b)) a c)))) *star
	 (*caten 2)
	 (*pack-with (lambda (a lambda_lst) 
		(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
done))



(define <InfixMulandDiv>
(^<skipped*>(new (*parser <Skip>) *star
     (*delayed (lambda () <InfixPow>))
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
     (*parser (char #\*))
	 (*parser (char #\/))
	 (*disj 2)
	 (*parser <Skip>) *star
	 (*delayed (lambda () <InfixPow>))
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
	 (*caten 2)
     (*pack-with (lambda (b c) (lambda (a) (list (string->symbol (string b)) a c)))) *star
	 (*caten 2)
	 (*pack-with (lambda (a lambda_lst) 
		(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
done)))

(define <InfixAdd> <InfixAddandSub>)
(define <InfixSub> <InfixAddandSub>)
(define <InfixDiv> <InfixMulandDiv>)
(define <InfixMul> <InfixMulandDiv>)

(define <InfixPow>
(^<skipped*>(new (*parser <Skip>) *star
     (*parser <Atom>)
	 (*delayed (lambda () <InfixNeg>))
	 (*delayed (lambda() <InfixSexprEscape>))
     (*disj 3)
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
     (*parser <PowerSymbol>)
	 (*parser <Skip>) *star
	 (*delayed (lambda () <InfixPow>))
	 (*parser <Atom>)
	 (*delayed (lambda () <InfixNeg>))
	 (*delayed (lambda() <InfixSexprEscape>))
	 (*disj 4)
	 (*parser <Skip>) *star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
	 (*caten 2)
     (*pack-with (lambda (b c) (lambda (a) (list 'expt a c)))) *star
	 (*caten 2)
	 (*pack-with (lambda (a lambda_lst) 
		(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
done)))


(define <InfixArrayGet>
(new ;(*parser <InfixAddandSub>)
     
     (*parser (char #\[))
	 (*parser <InfixAddandSub>)
	 (*parser (char #\]))
	 (*caten 3)
     (*pack-with (lambda (b c d)(lambda (a) (list 'vector-ref a c)))) 
	 
	 done))

(define <InfixArgList>
(^<skipped*>(new
(*parser <InfixAddandSub>)
(*parser (char #\,))
(*parser <InfixAddandSub>)
(*caten 2)
(*pack-with (lambda (a b) b)) *star
(*caten 2)
(*pack-with (lambda(a b) (cons a b)))
(*parser <epsilon>)
(*disj 2)
done)))

(define <InfixFuncall>
(^<skipped*>(new (*parser (char #\())
	 (*parser <InfixArgList>)
	 (*parser (char #\)))
	 (*caten 3)
     (*pack-with (lambda (b c d)(lambda (a) (cons a c)))) 
	 done)))
	 	 
(define <InfixParen>
(new (*parser (char #\())
     (*parser <InfixAddandSub>)
	 (*parser (char #\)))
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
	 done))	 
	 
(define <InfixPrefixExtensionPrefix>
(^<skipped*>(new (*parser (word-ci "##"))
     (*parser (word-ci "#%"))
	 (*disj 2)
done)))

(define <InfixSexprEscape>
(new (*parser <Skip>)*star
     (*parser <InfixPrefixExtensionPrefix>)
	 (*parser <Skip>)*star
	 (*caten 3)
	 (*pack-with (lambda (a b c) b))
     (*delayed (lambda () <sexpr>))
	 (*parser <Skip>)*star
	 (*caten 2)
	 (*pack-with (lambda (a b) a))
	 (*caten 2)
	 (*pack-with (lambda (a b) b))
done))

(define <InfixExpression>
(^<skipped*>(new 
     ;(*parser <InfixSexprEscape>)
	 (*parser <InfixAdd>)
     (*parser <InfixNeg>) 
	 (*parser <InfixSub>)
	 (*parser <InfixMul>)
	 (*parser <InfixDiv>)
	 (*parser <InfixPow>)
	 ;(*parser <InfixArrayGet>)
	 (*parser <InfixFuncall>)
	 (*parser <InfixParen>)
	 (*parser <InfixSymbol>)
	 (*parser <Number>)
     (*disj 10)
done)))






(define <InfixExtension>
(new (*parser <InfixPrefixExtensionPrefix>)
     (*parser <InfixExpression>)
	 (*caten 2)
	 (*pack-with (lambda (a b) b))
	 done))





  
(define <sexpr>
(^<skipped*>
  (new     (*parser <Boolean>)
           (*parser <Char>)
           (*parser <Number>)  
           (*parser <Symbol>)
           (*parser <digit-0-9>)
           *diff
           *not-followed-by
           (*parser <Symbol>) 
       (*parser <String>)  	   
       (*parser <ProperList>) 
       (*parser <ImproperList>) 
       (*parser <Vector>) 
       (*parser <Quoted>) 
       (*parser <QuasiQuoted>) 
	   (*parser <UnquoteAndSpliced>) 
       (*parser <Unquoted>) 
       (*parser <InfixExtension>)
     ;  (*parser <Skip>) 
       (*disj 13)
       done)))

       
(define <sexpr*> 
    (new (*parser <sexpr>) *plus
    done))



(load "pattern-matcher.scm")

	
(define simple-const?
  (lambda (sexpr)
  (or (vector? sexpr) (boolean? sexpr) (char? sexpr) (number? sexpr) (null? sexpr) (string? sexpr))))

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define *void-object* (if #f #f))

(define reserved-words?
(lambda (word) (ormap (lambda (expr) (eq? expr word)) *reserved-words* )))

(define var?
   (lambda (v)
     (and (symbol? v) (not (reserved-words? v)))))   

(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (car s))
     		(else `(begin ,@s)))))

(define ret-simple 
    (lambda (argl)
        `(lambda-simple ,argl)))
		
(define ret-opt (lambda (argl opt)
     `(lambda-opt ,argl ,opt )))
	 
(define ret-var (lambda (argl)
     `(lambda-var ,argl )))
		
			


(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      ;;;TODO: var?
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))					

(define beginloop
	(lambda(arg)
		(fold-left (lambda(acc x) 
		(if (and (list? x) (equal? 'begin (car x)))
			(append acc (beginloop (cdr x)))
			`(,@acc ,x)) 
			             ) '() arg)))
						 
(define makeSet (lambda (lst1 lst2)

                     (if (or(null? lst1) (null? lst2))
                          lst1
                          (append (list `(set! ,(car lst1) ,(car lst2)) ) (makeSet (cdr lst1) (cdr lst2)))
				  )))					  

(define flatVars (lambda (var rest)
                   
				   (if (null? rest)
				   (list var)
                   (cons var (map (lambda (rest)(car rest)) rest)))))
				   
(define flatVals (lambda (val rest)      
				   (if (null? rest)
				   (list val)
                   (cons val (map (lambda (rest)(car(cdr rest))) rest)))))

(define makeFalse (lambda (lst)
			       (if (null? lst)
					lst
					(append (list(list (car lst) #f)) (makeFalse (cdr lst))))))
					
(define makeAnd (lambda (first rest)

                   (if(= (length rest) 1)
				       `(if ,first ,@rest #f)					  
					   `(if ,first ,(makeAnd (car rest) (cdr rest)) ,#f))))
					   
;(define check
;	(lambda (lst) 
;	(cons (car lst) (filter (lambda (x) (display x )(display "__")(not(equal? x (car lst)))) lst))))
					   
(define correct? (lambda (lst)
	(if (var? lst)
		#t
		(if (and(pair? lst) (not (list? lst)))
			#t
			(if (null? lst) 
				#t
				(if (not (= (length lst) (length (cons (car lst) (filter (lambda (x) (not(equal? x (car lst)))) lst))))) 
					#f
					(correct? (cdr lst))))))))
  
					   
					   
					   
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
					   
					   
(define makeCond 
  (lambda (exprs) 

    (cond  ((and (not (equal? exprs `())) (list? (car exprs)) (not (equal? (caar exprs) 'else)))
           `(if ,(caar exprs) ,@(cdar exprs) ,(makeCond (cdr exprs))))
           ((and (not (equal? exprs '())) (equal? (caar exprs) 'else)) (cadar exprs)))
    ))
	


(define expand-cond
		(lambda (expr rest)
		(cond ((and (null? rest) (= (length expr) 2)(not(equal? 'else (car expr)))) `(if ,(car expr) ,(car (cdr expr))))
		      ((and (null? rest) ( > (length expr) 2) (not(equal? 'else (car expr)))) `(if ,(car expr) ,(beginify (cdr expr))))
			  ((and (not(null? rest)) (= (length expr) 2) (not(equal? 'else (car expr)))) `(if ,(car expr) ,(car(cdr expr)) ,(expand-cond (car rest) (cdr rest))))
			  ((and (not(null? rest)) ( > (length expr) 2)(not(equal? 'else (car expr)))) `(if ,(car expr) ,(beginify (cdr expr)) ,(expand-cond (car rest) (cdr rest))))
			  ((equal? 'else (car expr)) (beginify (cdr expr)))
			  )))
											   

					  


(define parse
	(let    ((run 
			(compose-patterns
				(pattern-rule
					(? 'c simple-const?)
					(lambda (c) `(const ,c)))
				(pattern-rule
					`(quote ,(? 'c))
					(lambda (c) `(const ,c)))
				(pattern-rule
					(? 'v var?)
					(lambda (v) `(var ,v)))
			    (pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				(pattern-rule
				    `(if ,(? 'test) ,(? 'dit))
					(lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))	
				(pattern-rule
					`(or ,(? 'expr))
					(lambda (expr)  (parse expr)))				
				(pattern-rule
					`(or ,(? 'expr) . ,(? 'exprs list?))
					(lambda (expr exprs)  `(or ,(map parse (cons  expr exprs)))))			
				(pattern-rule
					`(or)
					(lambda ()  (parse #f)))
                ;;DEFINE
				(pattern-rule 
					`(define ,(? 'exprs list?) ,(? 'expr) . ,(? 'body))
					(lambda (exprs expr body)  
					        `(def ,(parse (car exprs)) ,(parse `(lambda ,(cdr exprs) ,expr ,@body)))))
				(pattern-rule 
					`(define ,(? 'exprs pair?) ,(? 'expr) . ,(? 'body))
					(lambda (exprs expr body) 
					        `(def ,(parse (car exprs)) ,(parse `(lambda ,(cdr exprs) ,expr ,@body)))))
				(pattern-rule
				    `(define ,(? 'var) ( ,(? 'expr) . ,(? 'exprs)))		
					(lambda (var expr exprs) `(def ,(parse var) ,(parse (cons expr exprs)))))
				(pattern-rule
				    `(define ,(? 'var) ,(? 'expr) )		
					(lambda (var expr) `(def ,(parse var) ,(parse expr))))
				(pattern-rule 
					`(define ,(? 'var1 var?) ,(? 'var2 var?) ,(? 'var3 var?) . ,(? 'exprs))
					(lambda (var1 var2 var3 exprs) (if(not(null? exprs))  `(def ,(parse var1) ,(parse (beginify (list var2 var3 (car exprs)))))
					`(def ,(parse var1) ,(parse (beginify (list var2 var3)))))))

				(pattern-rule 
				    `(begin)
                     (lambda () `(const ,*void-object*)))
                (pattern-rule 
				     `(begin  ,(? 'begArg))
                     (lambda (begArg) (parse begArg)))
                (pattern-rule 
				     `(begin . ,(? 'seq list?))  ;list? is a "guard"
                     (lambda (seq) `(seq ,(map parse (beginloop seq)))))
			 	 ;;LAMBDA
				(pattern-rule
				    `(lambda ,(? 'expr correct?) . ,(? 'exprs list?))
					 (lambda (expr exprs) `(,@(identify-lambda expr ret-simple ret-opt ret-var) 
					 ,(parse (beginify  exprs)))))
					 
				;;LET
				(pattern-rule 
				     `(let () ,(? 'expr) . ,(? 'exprs list?))
						(lambda (expr exprs) 
					          (parse `(,`(lambda () ,@(cons expr exprs))))))
				(pattern-rule
				     `(let  ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
                      (lambda (var val rest exprs)
							(parse `(,`(lambda (,@(cons var (map (lambda (rest) (car rest)) rest))) ,@exprs) ,@(cons val(map (lambda (rest) (cadr rest)) rest)) ))
                               ;(parse  `(,1 ,2))
					  ))
                ;;let*
               (pattern-rule
					`(let* () ,(? 'expr) . ,(? 'exprs list?))
					(lambda (expr exprs) 
					
					(parse `((lambda () ,@(cons expr exprs)))  )))
				(pattern-rule
					`(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
					(lambda (var val rest exprs) (if(null? rest) (parse `(let ((,var ,val)) ,@exprs))
					(parse `(let ((,var ,val)) (let* ,rest . ,exprs))))))	
                ;;set!
                (pattern-rule
                     `(set! ,(? 'var) ,(? 'val))
                      (lambda (var val) `(set ,(parse var) ,(parse val))))
				
				(pattern-rule
				  `(letrec ((,(? 'var var?) ,(? 'val)) . ,(? 'rest ))  . ,(? 'exprs))
				   (lambda (var val rest exprs)
				   
					 (let* ((ListVar (flatVars var rest))
					        (ListVal (flatVals val rest))
					       (ListPairVarFalse (makeFalse ListVar))
						   (makePair (makeSet ListVar ListVal)))
						   ;(body (list  makePair (lambda () exprs))))
						   
						   (parse `(let  ,ListPairVarFalse ,@makePair ((lambda () ,@exprs)))
						   ))))
			  (pattern-rule
					`(letrec () ,(? 'expr) . ,(? 'exprs list?))
					 (lambda (expr exprs) 
					   (parse `(let() ((lambda(),expr ,@exprs))))))
					   
			;;AND
			(pattern-rule
					`(and ,(? 'expr))
					(lambda (expr)  (parse expr)))
            (pattern-rule 
					`(and ,(? 'expr) . ,(? 'exprs))
					(lambda (expr exprs) (parse (makeAnd expr exprs))))
			  (pattern-rule
					`(and)
					(lambda () (parse #t)))
	
			;;COND
			(pattern-rule
				`(cond ,(? 'expr) . ,(? 'rest))
				(lambda (expr rest) 
					(parse (expand-cond expr rest))))
			
			
			
           

				
			;;quasiquote
			(pattern-rule ;;quasi-quote
             `(quasiquote . ,(? 'args))
             (lambda (args)
               (parse (expand-qq (car args)))))
				
	
				(pattern-rule
				     `( ,(? 'app) . ,(? 'exprs list?))
					 (lambda (app exprs) `(applic ,(parse app) ,(map parse exprs))))
				;; add more rules here
				)))
			(lambda (sexpr)
				(run sexpr
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" sexpr)))))))
									
									
;;////////////ASS3/////////////////////

			
;//eliminated-nested-define//

(define lambda-simple?
	(lambda (expr)
				(eq? (car expr) 'lambda-simple)))
				
(define lambda-opt?
	(lambda (expr)
				(eq? (car expr) 'lambda-opt)))

(define lambda-var?
	(lambda (expr)
				(eq? (car expr) 'lambda-var)))
			
(define lambda? (lambda (expr) (and (list? expr)  (> (length expr) 1) (or (lambda-simple? expr) (lambda-opt? expr) (lambda-var? expr)))))


;/getting the body of a lambda
(define getBody
    (lambda (expr) 
	
        (cond ((or (lambda-simple? expr) (lambda-var? expr)) (cddr expr) )
				((lambda-opt? expr) (cdddr expr)))))
			   

(define creat_lambda_new (lambda (parsed-expr new-body) 

(if (or (lambda-simple? parsed-expr) (lambda-var? parsed-expr))
`( ,(car parsed-expr) ,(cadr parsed-expr) ,new-body )
`( ,(car parsed-expr) ,(cadr parsed-expr) ,(caddr parsed-expr) ,new-body))
	
	))
	

(define applic? (lambda (expr) 
    (and (list? expr) (not (null? expr)) (eq? (car expr) 'applic))))
	
(define applic-first-child (lambda (expr) (cadr expr)))

(define applic-rest-children (lambda (expr) (caddr expr)))
	
(define or? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'or))))
	
	
(define get-or-last-exp (lambda (expr)
;(display expr)
;(newline)
	(if (and (list? expr) (= (length expr) 1)) 
		(car expr)
		(get-or-last-exp (cdr expr)))))
		
(define get-or-exprs 

	(letrec ((helper (lambda (or-list-expr)

						(if (and (list? or-list-expr )(not (= (length or-list-expr) 1)))
							(cons (car or-list-expr) (helper (cdr or-list-expr)))
							'()))))
			(lambda (expr)
			(helper expr))))
	
(define if? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'if3))))
	
(define get-test-if (lambda (expr)
 (cadr expr)))
 
(define get-dit-if (lambda (expr) 
 (caddr expr)))

(define get-dif-if (lambda (expr)
 (cadddr expr)))
	

	

 
(define define? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'def))))
	
(define get-var-define (lambda (expr)
	(cadr expr)))
	
(define get-expr-define (lambda (expr) 
	(caddr expr)))
	
	
(define get-param-lambda (lambda (expr)
	(cadr expr)))
	
(define seq? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'seq))))
	
(define set? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'set))))

(define box-set? (lambda(expr) 
    (and (list? expr) (eq? (car expr) 'box-set))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define debug-mode-on #f)

;;; AVI HELPER FUNCTIONS ;;;
(define disp
  (lambda (exp)
    (display "\n")
    (display "The expression is: ")
    (display exp)
    (display "\n")))

;; when debug-level==0 i don't display this message.. it's less important..
;; a way to shut down certain debug messages temporarily..
(define display-debug
  (lambda (exp debug-level)
    (if (and (equal? debug-mode-on #t) (> debug-level 0))
        (display exp)
        (void))))

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

;;END OF AVI HELPER FUNCTIONS;;;


;(define eliminate-nested-defines
;	(lambda (expr)
;		(cond ((lambda? expr) 
;			(foo (getBody expr) 
;			   (lambda (ds es) 
;					(if (null? ds) 
;						(creat_lambda_new expr (car (map eliminate-nested-defines (getBody expr))))
;						(let*  ((setters (map (lambda (ds)`(set ,(cadr ds) ,(eliminate-nested-defines (caddr ds)))) ds))
;						       (vars (map (lambda (ds) (cadadr ds)) ds))
;							   (es (map eliminate-nested-defines es))
;							   (bodyOfLambdaSimple `(seq ,(append setters es)))
;							   (lambdaApplic `(lambda-simple ,vars ,bodyOfLambdaSimple))
;							   (eliminate-lambdaApplic  lambdaApplic)
;							   (falseVar (map (lambda (vars) '(const #f))  vars))
;							   (creatTheApplic `(applic ,eliminate-lambdaApplic ,falseVar)))

;                         (creat_lambda_new expr creatTheApplic))))))
;			 ((list? expr) (map eliminate-nested-defines expr))
;			 (else expr))))
									
		   
			   
;//MEYAR-FUNCTION-FROM-CLASS//

;(define foo
;    (lambda (pes ret-ds+es)
;        (if (null? pes) (ret-ds+es '() '())
;            (foo (cdr pes)
;                (lambda(ds es)
;                    (cond ((eq? (caar pes) 'def)
;                            (ret-ds+es (cons (car pes) ds) es))
;                            ((eq? (caar pes) 'seq)
;                            (foo (cadar pes)
;                              (lambda(ds1 es1)
;                              (ret-ds+es (append ds1 ds)
;                                        (append es1 es)))))
;                            (else (ret-ds+es ds (cons (car pes) es)))))))))


;;;; AVI:
;############################################## eliminate nested defines ##################################
		
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
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;ELIII -START ;;

;(define remove-applic-lambda-nil
;(let    ((run 
;			(compose-patterns
;				(pattern-rule
;					`(applic (lambda-simple() ,(? 'expr) ) () )
;					(lambda(expr) (remove-applic-lambda-nil expr))))))
;	(lambda (expr)
;
;		(run expr 
;		(lambda ()  (if (list? expr)
;							(map remove-applic-lambda-nil expr)
;							expr))))))
;;END-OF-ELII;;

;;AVI - START;;


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

;;AVI -END;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define v-not-in-params
	(lambda (v params)
          (begin (display-debug "in v-not-in-params\n" 1)
                 (display-debug "v is:" 1) (display-debug v 1) (display-debug "\n" 1)
                 (display-debug "params is:" 1) (display-debug params 1) (display-debug "\n" 1)
                 (let ((new-params (if (not (list? params)) (list params) params)))
		(not (ormap (lambda (vi) (eq? v vi)) new-params))))
	)
)

;; exp is the rest (meaning doesn't contain the tag
;(define get-lambda-opt-body
;  (lambda (rest)
;    (caddr rest)
;    ))

;; exp is the rest (meaning doesn't contain the tag
;(define get-lambda-opt-params
;  (lambda (rest)
;    (append (get-lambda-parameters rest) (list (cadr rest)))
;    ))

(define is-bound
	(lambda (v body)
          (begin (display-debug "hello im in is-bound \n" 1)
          (display-debug "v: " 1) (display-debug v 1) (display-debug "\n" 1)
          (display-debug "body: " 1) (display-debug body 1) (display-debug "\n" 1)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
                          
			(cond
                                  ((eq? tag 'lambda-simple)
                                   (begin (display-debug "case0\n" 1)
                                          (if (v-not-in-params v (get-lambda-parameters rest)) (has-get v (get-lambda-body rest)) #f)))

                                  ((eq? tag 'lambda-opt)
                                   (begin (display-debug "case0.1\n" 1)
                                          (if (v-not-in-params v (append (get-lambda-parameters rest) (list (cadr rest)))) (has-get v (caddr rest)) #f)))
                                  
                                  ;;what i need to do next! row above or below - why doesn't return #t????
                                  
                                  ((eq? tag 'lambda-var) (begin (display-debug "case1\n" 1)
                                                                (if (v-not-in-params v (list (get-lambda-parameters rest))) (has-get v (get-lambda-body rest)) #f)))
                                                                ;(is-bound v (get-lambda-body rest))))
                                  
				  ((eq? tag 'seq) (begin (display-debug "case2\n" 1)
                                                         (ormap (lambda (expr) (is-bound v expr)) (get-tatey-expr rest))))
                                  
				  ((eq? tag 'applic) (begin (display-debug "case3\n" 1)
                                                            (or (is-bound v (get-applic-operator rest))
                                                                (ormap (lambda (expr) (is-bound v expr)) (get-applic-operands rest)))))
                                  
				  ((eq? tag 'def) (begin (display-debug "case4\n" 1)
                                                         (or (is-bound v (get-define-var rest)) 
                                                             (is-bound v (get-define-val rest)))))
                                  
				  ((eq? tag 'if3) (begin (display-debug "case5\n" 1)
                                                         (or (is-bound v (get-if-condition rest)) 
                                                             (is-bound v (get-if-first rest)) 
                                                             (is-bound v (get-if-second rest)))))
                                  
				  ((eq? tag 'or) (begin (display-debug "case6\n" 1)
                                                        (ormap (lambda (expr) (is-bound v expr)) (get-or-sub-exprs rest))))
				  
				  ;;;;;((eq? tag 'set) `(,tag ,(is-bound v (get-set-var rest)) ,(is-bound v (get-set-val rest))))
                                  ((eq? tag 'set) (begin (display-debug "case7\n" 1)
                                                         ;;(or (is-bound (get-set-var rest))
                                                         (is-bound v (get-set-val rest))))
				  
				  (else (begin (display-debug "case else\n" 1) #f))
				))
		)
	))
)

(define has-set
	(lambda (v body)
          (begin (display-debug "in has-set\n" 1) (display-debug "v is:" 1) (display-debug v 1) (display-debug "\n" 1) (display-debug "body is:" 1) (display-debug body 1) (display-debug "\n" 1)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond ((eq? tag 'set) (eq? v (cadar rest)))
				  ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest))) (has-set v (get-lambda-body rest)))
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
				  
				  ;;;;;((eq? tag 'set) `(,tag ,(has-set (get-set-var rest)) ,(has-set (get-set-val rest))))
				  
				  (else #f)
				)))
		)
	)
)

(define has-get
	(lambda (v body)
          (begin (display-debug "in has-get\n" 1) (display-debug "v is:" 1) (display-debug v 1) (display-debug "\n" 1) (display-debug "body is:" 1) (display-debug body 1) (display-debug "\n" 1)
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
				  ((eq? tag 'set) (or (has-get v (get-set-var rest)) (has-get v (get-set-val rest))))
				  (else #f)
				))
		))
	)
)

(define check-box-criteria 
	(lambda (parameter body)
          (begin (display-debug "in check-box-criteria\n" 1)
	;(disp 3)
          (let ((first-check (is-bound parameter body))
                (second-check (has-set parameter body))
                (third-check (has-get parameter body)))
            (begin  (display-debug "first-check is:" 1) (display-debug first-check 1) (display-debug "\n" 1)
                    (display-debug "second-check is:" 1) (display-debug second-check 1) (display-debug "\n" 1)
                    (display-debug "third-check is:" 1) (display-debug third-check 1) (display-debug "\n" 1)
                    
                    (and (is-bound parameter body) (has-set parameter body) (has-get parameter body))))
	))
)

 
(define add-first-set
	(lambda (v body)
          (begin (display-debug "in add-first-set\n" 1)
		(if (eq? 'seq (car body))
			`(seq ((set (var ,v) (box (var ,v))) ,@(get-tatey-expr (cdr body))))
			`(seq ((set (var ,v) (box (var ,v))) ,(box-set body)))
		))
	)
)

;;receives a param v and a body
;; 
(define replace-get
	(lambda (v body)
         (begin (display-debug "hello im in replace-get \n" 1)
          (display-debug "v: " 1) (display-debug v 1) (display-debug "\n" 1)
          (display-debug "body: " 1) (display-debug body 1) (display-debug "\n" 1)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
                          (begin (display-debug "tag:" 1) (display-debug tag 1) (display-debug "\n" 1)
                          (display-debug "rest:" 1) (display-debug rest 1) (display-debug "\n" 1)
                          
			(cond
                                  ;;  body == (var x) ==> tag='var , rest= '(x), so if (car rest)==v then we replace this with a box-get record.. (box-get (var 
                                  ((eq? tag 'var) (begin (display-debug "case0\n" 1)
                                                     (if (eq? v (car rest)) `(box-get ,body) body)))
                               
                                  ((and (eq? tag 'lambda-simple) (v-not-in-params v (get-lambda-parameters rest)))
                                                 (begin (display-debug "case1.1\n" 1)
						`(,tag ,(get-lambda-parameters rest) ,(replace-get v (get-lambda-body rest)))))

                                  ((and (eq? tag 'lambda-var) (v-not-in-params v (list (get-lambda-parameters rest))))
                                                 (begin (display-debug "case1.2\n" 1)
						`(,tag ,(get-lambda-parameters rest) ,(replace-get v (get-lambda-body rest)))))
                                  
                                 ; ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest)))
                                 ;                (begin (display-debug "case1\n" 1)
				;		`(,tag ,(get-lambda-parameters rest) ,(replace-get v (get-lambda-body rest)))))
                                  
				  ((eq? tag 'lambda-opt) (begin (display-debug "case2\n" 1)
                                                                `(,tag ,(get-lambda-parameters rest) ,(get-lambda-opt-params rest) ,(replace-get v (get-lambda-opt-body rest)))))
                                  
				  ((eq? tag 'seq) (begin (display-debug "case3\n" 1)
                                                         `(,tag ,(map (lambda (expr) (replace-get v expr)) (get-tatey-expr rest)))))
                                  
				  ((eq? tag 'applic) (begin (display-debug "case4\n" 1)
                                                            `(,tag ,(replace-get v (get-applic-operator rest)) ,(map (lambda (expr) (replace-get v expr)) (get-applic-operands rest)))))
                                  
				  ((eq? tag 'def) (begin (display-debug "case5\n" 1)
                                                         `(,tag ,(replace-get v (get-define-var rest)) ,(replace-get v (get-define-val rest)))))
                                  
				  ((eq? tag 'if3) (begin (display-debug "case6\n" 1)
                                                         `(,tag ,(replace-get v (get-if-condition rest)) ,(replace-get v (get-if-first rest)) ,(replace-get v (get-if-second rest)))))
                                  
				  ((eq? tag 'or) (begin (display-debug "case7\n" 1)
                                                        `(,tag ,(map (lambda (expr) (replace-get v expr)) (get-or-sub-exprs rest)))))
                                  
				  ((eq? tag 'set) (begin (display-debug "case8\n" 1)
                                                         `(,tag ,(replace-get v (get-set-var rest)) ,(replace-get v (get-set-val rest)))))
                                  
                                  ;; body == (set a b) => tag=set, rest=(a b)
                                  ((eq? tag 'box-set) (begin (display-debug "case9\n" 1)
                                                             `(,tag ,(get-set-var rest) ,(replace-get v (get-set-val rest)))))
                                  
				  (else (begin (display-debug "case10\n" 1) body))
				)))
		)
	))
)

(define replace-set
	(lambda (v body)
          (begin (display-debug "hello im in replace-set \n" 1)
          (display-debug "v: " 1) (display-debug v 1) (display-debug "\n" 1)
          (display-debug "body: " 1) (display-debug body 1) (display-debug "\n" 1)
		(if (null? body) body
			(let
				((tag (car body))
				 (rest (cdr body)))
			(cond
                                  ((eq? tag 'set) (begin (display-debug "case0\n" 1)
                                                         (if (eq? v (cadar rest)) `(box-set ,@rest) body)))
                                  
                                  ((and (or (eq? tag 'lambda-simple) (eq? tag 'lambda-var)) (v-not-in-params v (get-lambda-parameters rest)))
                                                  (begin (display-debug "case1\n" 1)
                                                         `(,tag ,(get-lambda-parameters rest) ,(replace-set v (get-lambda-body rest)))))
                                  
				  ((eq? tag 'lambda-opt) (begin (display-debug "case2\n" 1)
                                                                `(,tag ,(get-lambda-parameters rest) 
                                                                       ,(get-lambda-opt-params rest) 
                                                                       ,(replace-set v (get-lambda-opt-body rest)))))
                                  
				  ((eq? tag 'seq) (begin (display-debug "case3\n" 1)
                                                         `(,tag ,(map (lambda (expr) (replace-set v expr)) (get-tatey-expr rest)))))
                                  
				  ((eq? tag 'applic) (begin (display-debug "case4\n" 1)
                                                            `(,tag ,(replace-set v (get-applic-operator rest))
                                                                   ,(map (lambda (expr) (replace-set v expr)) (get-applic-operands rest)))))
                                  
				  ((eq? tag 'def) (begin (display-debug "Case4\n" 1)
                                                         `(,tag ,(replace-set v (get-define-var rest)) 
                                                                ,(replace-set v (get-define-val rest)))))
                                  
				  ((eq? tag 'if3) (begin (display-debug "case5\n" 1)
                                                         `(,tag ,(replace-set v (get-if-condition rest)) 
                                                                ,(replace-set v (get-if-first rest))
                                                                ,(replace-set v (get-if-second rest)))))
                                  
				  ((eq? tag 'or) (begin (display-debug "Case6\n" 1)
                                                        `(,tag ,(map (lambda (expr) (replace-set v expr)) (get-or-sub-exprs rest)))))
                                  
				  (else (begin (display-debug "case else" 1) body))
				)))
		)
	)
)

;; receives a param and body
;; first calls replace set with v and body
;; so we have a new body with all occurences of (set v _) replaced with (box-set v _)
;; then we call replace-get v with this new body..
;; so we replace all occurences of reading!! v with (box-get v)
;; then we call box-set on this new body we created..
;; what is add-first-set
(define let-us-box
	(lambda (v body)
          (begin (display-debug "inside let-us-box\n" 1)
                 (display-debug "v is:" 1) (display-debug v 1) (display-debug "\n" 1)
                 (display-debug "body is:" 1) (display-debug body 1) (display-debug "\n" 1)
                 ;(disp 3)
                 (box-set (add-first-set v (box-set (replace-get v (box-set (replace-set v body))))))
                 ))
  )


;; here we get params of a lambda and it's body
;; v is the first of the params, if (check-box-criteria v body) is true then we call (do-boxing (rest params) and also do (let-us-box body)
;; else we call set-box on the body..
;; don't understand why we box only the first parameter..

;; else, we just call 
(define do-boxing2
	(lambda (params body)
          (begin
            ; (display "do-boxing!!\n")
          ;(display "params:") (display params) (display "\n")
         ; (display "body:") (display body) (display "\n")
	;(disp 2)
		(cond ((null? params) body)
			  (else (let ((v (car params)))
                                  (begin (display "hello, v is:") (display v) (display "\n")
						(if (check-box-criteria v body)
							(do-boxing (cdr params) (let-us-box v body))
                                                        ;;(do-boxing (cdr params) body)
						(box-set body)

                                                        ))
					)
			  )
		)
	))
)

;; maayan version
(define do-boxing
  (lambda (params body)
    (begin
      (display-debug "inside do-boxing\n" 1)
          (display-debug "params:" 1) (display-debug params 1) (display-debug "\n" 1)
          (display-debug "body:" 1) (display-debug body 1) (display-debug "\n" 1)
          (cond ((null? params) (box-set body)) ;;personally i think this should be (box-set body) and not body but the tests prefer body.. don't know why
                (else
                 (let ((v (car params)))
                   (begin
                     (display-debug "v is:" 1) (display-debug v 1) (display-debug "\n" 1)
                          (if (check-box-criteria v body)
                              (begin (display-debug "according to check-box-criteria should box param " 1) (display-debug v 1) (display-debug "\n" 1) (do-boxing (cdr params) (let-us-box v body)))
                              (begin (display-debug "according to check-box-criteria shouldn't box param "1) (display-debug v 1) (display-debug "\n" 1) (do-boxing (cdr params) body))
                              ))))))))
          

;; so if the expr is null we just return it..
;; we then calc (car expr) as "tag" and "rest" as (cdr expr)
;; 
(define box-set 
	(lambda (expr)
          (begin (display-debug "inside box-set\n" 1)
          (display-debug "current expr:" 1) (display-debug expr 1) (display-debug "\n" 1)
	;(disp 1)
		(if (null? expr) expr
			(let
				((tag (car expr))
				 (rest (cdr expr)))

			(cond
                              ;; if this is a seq then go over the 'rest' and for every exp perform box-set
                              ((eq? tag 'seq) (begin (display-debug "case 0" 1)
                                                     `(,tag ,(map (lambda (tat-expr) (box-set tat-expr)) (get-tatey-expr rest))))) 
                              
                              ;; if this is an applic record, do a box set for the op
                              ;; then do a box set of the operands
                              ;; example, for (+ 1 2) (applic (var +) ((const 1) (const 2))) first do (box-set (var +)) then do (box-set ((const 1) (const 2)))
                              
                              ((eq? tag 'applic) (begin (display-debug "case1\n" 1)
                                                        `(,tag ,(box-set (get-applic-operator rest))
                                                        ,(map (lambda (operand) (box-set operand)) (get-applic-operands rest)))))
                              
                              ;;for a define record, do box-set on the var then do a box-set on the val
                              ((eq? tag 'def) (begin (display-debug "case2\n" 1)
                                                     `(,tag ,(box-set (get-define-var rest)) 
                                                            ,(box-set (get-define-val rest)))))
                              
                              ;; for an if record, do a box-set on the condition, then on the then then and the else
                              ((eq? tag 'if3) (begin (display-debug "case3\n" 1)
                                                     `(,tag ,(box-set (get-if-condition rest)) 
                                                            ,(box-set (get-if-first rest)) 
                                                            ,(box-set (get-if-second rest)))))
                              
                              ;; for an or record, do a box-set on every statement seperatly
                              ((eq? tag 'or) (begin (display-debug "case4\n" 1)
                                                    `(,tag ,(map (lambda (tat-expr) (box-set tat-expr)) (get-or-sub-exprs rest)))))

                              ;; for a lambda-simple exp, call do-boxing with the parmeters and the body
                              ((eq? tag 'lambda-simple) (begin (display-debug "case5\n" 1)
                                                               `(,tag ,(get-lambda-parameters rest) ,(do-boxing (get-lambda-parameters rest) (get-lambda-body rest)))))

                              ;; for a lambda-opt call do-boxing with the parmeters and the body
                              ((eq? tag 'lambda-opt) (begin (display-debug "case6\n" 1)
                                                            `(,tag ,(get-lambda-parameters rest) ,(get-lambda-opt-params rest) ,(do-boxing (get-lambda-parameters rest) (get-lambda-opt-body rest)))))

                              ;;for a lambda-var, call box-set for the body..
                              ;; why don't we need to box here???
                              ((eq? tag 'lambda-var) (begin (display-debug "case7\n" 1)
                                                            `(,tag ,(get-lambda-parameters rest) ,(box-set (get-lambda-body rest)))))
				  
				  ;;;;;((eq? tag 'set) `(,tag ,(box-set (get-set-var rest)) ,(box-set (get-set-val rest))))
				  
                              (else (begin (display-debug "case8\n" 1) expr))
				)))
		)
	)	
)

;(define is-arg-bound?
;    (lambda (arg body)
;        (cond 
;            ((null? body) #f)
;            ((not(list? body)) #f)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f)
;            ((and (is-lambda? body) (not(arg-in-param arg (get-params body)))) 
;                                                            (is-arg-really-bound? arg (car(cdr(cdr body)))))
;            (else (ormap (lambda(x)(is-arg-bound? arg x)) body) )
;        )
;))

;looking for appearance of "(var arg)" (which is not inside a problematic lambda)
;(define is-arg-really-bound? 
;    (lambda (arg body)
;        (cond 
;            ((null? body) #f)
;            ((not(list? body)) #f)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f)
;            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg)) #t)
;            (else 
;             (ormap (lambda(x)(is-arg-really-bound? arg x)) body)
;            )
;        )
;))

;(define arg-is-SET? 
;    (lambda (arg body)
;        (cond 
;            ((null? body) #f)
;            ((not(list? body)) #f)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f) ;; need to lehatim also to lambda opt and var
;            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) #t)
;            (else 
;             (ormap (lambda(x)(arg-is-SET? arg x)) body)
;            )
;        )
;))

;(define arg-is-GET? 
;    (lambda (arg body)
;        (cond 
;            ((null? body) #f)
;            ((not(list? body)) #f)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body)) #f)) ;; need to lehatim also to lambda opt and var
;            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
;                (arg-is-GET? arg (cdr(cdr body)))) 
;            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg)) #t) ; maybe
;            (else 
;             (ormap (lambda(x)(arg-is-GET? arg x)) body)
;            )
;        )
;))

;(define get-params
;	(lambda (exp)
;        (let*(
;            (type-of-lambda (car exp))
;            (args (cond 
;                    ((equal? type-of-lambda 'lambda-simple) (car (cdr exp)))
;                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
;                    ((equal? type-of-lambda 'lambda-opt) `( ,@(car(cdr exp)) ,(car(cdr(cdr exp))))    )
;                    
;                    ))
;;            
;            )
;        args))
;	)

;(define get-body
;	(lambda (exp) 
;        (let*(
;            (type-of-lambda (car exp))
;            
;            (body (cond 
;                    ((equal? type-of-lambda 'lambda-simple) (car(cdr(cdr exp))))
;                    ((equal? type-of-lambda 'lambda-var) (car(cdr(cdr exp))))
;                    ((equal? type-of-lambda 'lambda-opt) (car(cdr(cdr(cdr exp)))))
;                    ))
;            )
;        body))
;
;	)


;(define arg-in-param
;    (lambda (arg args)
;        (if (member arg args) 
;            #t
;            #f
;)))

                                    
                                    
;(define smallest? 
;       (lambda(exp)
;       (not(ormap (lambda(x)(and(list? x)(not(quote? x)))) exp))))
      
      


;(define is-lambda?
;    (lambda (exp)
;        (or (lambda-simple-exp? exp)(lambda-opt-exp? exp)(lambda-variadic-exp? exp))
;        ))
   


;(define lambda-simple-exp?
;  (lambda(exp)
;    (and (pair? exp) (equal? (car exp) 'lambda-simple))))
    
;(define lambda-opt-exp?
;  (lambda(exp) 
;    (and (pair? exp) (equal? (car exp) 'lambda-opt))))

;(define lambda-variadic-exp?
;  (lambda(exp)
;    (and (pair? exp) (equal? (car exp) 'lambda-var))))
	

	
	
;(define box-set
;    (lambda (exp)
;        (cond 
;            ((null? exp) exp)
;            ((not(list? exp)) exp)           
;            ((is-lambda? exp) (lambda-helper exp))
;            ((null?(cdr exp)) (list(box-set (car exp)))) 
;            ((null?(car exp)) (list(box-set (cdr exp))))
;            ;; need to fix!!! what Elad said was true.. there are more cases to check. Elad sent me in facebook some reference
;            (else 
;            ;(map (lambda(x)(box-set exp)) exp)
;            (cons (box-set (car exp)) (box-set (cdr exp)))
;            ;(map (box-set (car exp)) (box-set (cdr exp))))
;        )   
;)))



; get list of args and body
; returns: body updated
;(define lambda-and-params-helper
;    (lambda (params body)
;        (cond 
;            ((null? params) body)
;            ((not(list? params)) body)
;            ((=(length params)0) body)
;            ((=(length params)1)
;            ; (begin (display params) (display 'asdasdasd)
;             	(box-arg-if-needed (car params) body))
;             ;)
;            (else 
;            (lambda-and-params-helper (cdr params) (box-arg-if-needed (car params) body))
;            )
;        )   
;))

;get lambda-exp
;NEED TO FIX THE ACCESS TO ARGS IN LAMBDA OPT AND VAR
;(define lambda-helper
;    (lambda (exp)
;        (let*(
;            (type-of-lambda (car exp))
;            (original-args
;            	(cond 
;                    ((equal? type-of-lambda 'lambda-simple) (list(car (cdr exp))))
;                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
;                    ((equal? type-of-lambda 'lambda-opt) (list (car(cdr exp)) (car(cdr(cdr exp)))))
;                    )
;            	)
;            (args (cond 
;                    ((equal? type-of-lambda 'lambda-simple) (car (cdr exp)))
;                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
;                    ((equal? type-of-lambda 'lambda-opt) `( ,@(car(cdr exp)) ,(car(cdr(cdr exp))))    )
;                    ))
;            (args-final
;                (reverse args)
;            )
;            (body (cond 
;                    ((equal? type-of-lambda 'lambda-simple) (car(cdr(cdr exp))))
;                    ((equal? type-of-lambda 'lambda-var) (car(cdr(cdr exp))))
;                    ((equal? type-of-lambda 'lambda-opt) (car(cdr(cdr(cdr exp)))))
;                    ))
;            (body-final
;                (box-set body)
;                )
;            )
;            (if(= 0(length args))
;            `(,type-of-lambda,@original-args, body-final)
            ;(begin (display 'type: ) (display type-of-lambda)(display ' agrs: ) (display args) 
;            	`(,type-of-lambda,@original-args, (lambda-and-params-helper args-final body-final))
            ;	)
;            )      
;)))


;(define box-arg-if-needed
;    (lambda (arg body)
;         (if
;         (and(arg-is-GET? arg body)(is-arg-bound? arg body)(arg-is-SET? arg body))
;         (make-changes arg body)
;         body
;         )
;))

;(define make-changes 
;    (lambda (arg body)
;        (make-changes-add-expression arg (make-changes-set arg (make-changes-get arg body)))
;    )
;)



;(define make-changes-add-expression
;    (lambda (arg body)
;      (let(
;        (header `(set, (list 'var arg), (list 'box (list 'var arg))))
;        )
;        (if(equal? (car body) 'seq)
;            `(seq,`(,header,@(car(cdr body))))
;            `(seq, `(,header, body))
;
;            ))))

;(define make-changes-set
;    (lambda (arg body)
;         (cond 
;            ((null? body) body)
;            ((not(list? body)) body)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body))) body) ;; need to lehatim also to lambda opt and var
;            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
;                                                                                            `(box-set, (car(cdr body)), (car(cdr(cdr body))) ))
;            (else 
;             (cons (make-changes-set arg (car body)) (make-changes-set arg (cdr body)))
;            )
;        )
;))





;(define make-changes-get
;    (lambda (arg body)
;         (cond 
;            ((null? body) body)
;            ((not(list? body)) body)
;            ((and (is-lambda? body) (arg-in-param arg (get-params body))) body) ;; need to lehatim also to lambda opt and var
;            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
;                `(set, (list 'var arg),@(make-changes-get arg (cdr(cdr body))))) 
;            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg))
;                                                                                            `(box-get, (list 'var arg) ))
;            (else 
;             (cons (make-changes-get arg (car body)) (make-changes-get arg (cdr body)))
;            )
;        )
;))


;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
(define is-member-of-single-list 
	(lambda (lst var)
		 (if (null? lst) 
			#f
			(if (eq? var (car lst)) 
				#t
			   (is-member-of-single-list (cdr lst) var)))))

(define is-in-env (lambda (lst var)
	(if(null? lst) #f
		(if(is-member-of-single-list (car lst) var) #t
			(is-in-env (cdr lst) var)))))
			   
         

(define getLambdaSimPleVars (lambda (expr)(cadr expr)))
(define getLambdaVarVars (lambda (expr) (cond((lambda-var? expr) (list (cadr expr))))))
(define getLambdaOptVars (lambda (expr)(append (cadr expr) (list (caddr expr)))))

                                       
                                                              
               
(define calc_place
        (lambda (var lst)
        (letrec ((helper (lambda(lst var n)
                        (if (null? lst) #f
						;(if (eq? (is-in-env lst var) #f) #f
                        (if (eq? (car lst) var) n
                        (helper (cdr lst) var (+ n 1)))))))
                        (helper lst var 0))))
                              
                                        
(define serch (lambda (var lst)
                (letrec ((helper-find (lambda(lst n)

                                    (if (null? lst) `(fvar ,var)
                                    (let ((place (calc_place var (car lst))))
                                    (if (not (eq? place #f))
										(if (= n -1)
											`(pvar ,var ,place)
											`(bvar ,var ,n ,place))
                                    (helper-find (cdr lst) (+ n 1))
                                    ))))))
                       (helper-find lst -1))))


               
;(define pe->lex-pe
;                    (letrec ((helper-pe (lambda (expr lst) 
                    
;					(if (and (lambda? expr)(lambda-simple? expr))
;                        (let ((vars (getLambdaSimPleVars expr))
;                             (body (get-body expr)))
							
;                             (creat_lambda_new expr  (helper-pe body (cons vars lst))))
;					(if (and (lambda? expr)(lambda-var? expr))
;                        (let ((vars (getLambdaVarVars expr))
;                            (body (get-body expr)))
							 
;                             (creat_lambda_new expr (helper-pe body (cons vars lst))))
;				    (if (and (lambda? expr)(lambda-opt? expr))
;                        (let ((vars (getLambdaOptVars expr))
;                             (body (get-body expr)))
;							 
;                            (creat_lambda_new expr  (helper-pe body (cons vars lst))))
;					(if (applic? expr)
;                    `(applic ,(helper-pe (applic-first-child expr) lst) ,(map (lambda(x) (helper-pe x lst) ) (applic-rest-children expr)))
;                    (if (and (list? expr) (not (null? expr)))
;                        (if (eq? 'var (car expr))
;                            (serch (cadr expr) lst)
;                            (map (lambda(x) (helper-pe x lst)) expr)) expr))))))))
;                            (lambda (sexpr)
;				(helper-pe sexpr '() ))          
;                            ))

			

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; MAAYAN -- START
;;; Assignment 3 -  Task 6 ;;;
(define get-lambda-params
  (lambda (exp)
    (cond
      ((equal? (car exp) 'lambda-simple) (cadr exp))
      ((equal? (car exp) 'lambda-opt) (append (cadr exp) (list (caddr exp))))
      ((equal? (car exp) 'lambda-var) (cadr exp))
    ;(cadr exp) 
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

;;Pre-Condition: lst is a list whose every element is a list 
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
   ; (begin (display "in make-lexical-var")
    (let ((var-name (cadr var-record)))
    (cond ((equal? (member-first? var-name vars-lst) #f) `(fvar ,var-name) ) ;;this is the case that var is free
          (else
           (let* ((var-record (member-first? var-name vars-lst))
                    (var-major (cadr var-record))
                    (var-minor (caddr var-record))
                    (major-diff (- current-major var-major)))
             (if (equal? major-diff 0)
                 `(pvar ,var-name ,var-minor) ;;this is a parameter!
                 `(bvar ,var-name ,(- major-diff 1) ,var-minor) ;;this is a bounded
             )))
          ))
    ;)
    
    ))

(define element-index
  (lambda (item lst)
   ; (begin (display "in element-index\n")
      (- (length lst) (length (memv item lst)) )
    ;)
    
    ))

(define update-vars-lst
  (lambda (vars-lst lambda-params current-major)
  ;  (begin (display "in update-vars-lst: \n")
         ;  (display "vars-lst:") (display vars-lst) (display "\n")
         ;  (display "lambda-params:") (display lambda-params) (display "\n")
          ; (display "current-major:") (display current-major) (display "\n")
    (let* ((new-lambda-params (if (not (list? lambda-params)) (list lambda-params) lambda-params))
          
           (new-vars-lst  (remove-elements-from-lst new-lambda-params vars-lst)))
      
      (append new-vars-lst (map (lambda (item) (list item current-major (element-index item new-lambda-params))) new-lambda-params)))
          
    ;)
           
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

(define annotate-lexical-debug
  (lambda (exp vars-lst current-major)
    (display "-----------------------\n")
    (display "new exp: ") (display exp) (display "\n")
    (display "vars-lst: ") (display vars-lst) (display "\n")
    (display "current-major: ") (display current-major) (display "\n")
         (cond
            ((or (not (list? exp)) (equal? exp '())) (begin (display "case1 ") exp)) 
            ((lambda-3? exp) (begin (display "case2 ") (display "exp is:") (display exp) (display "\n")
             (let ((new-vars-lst (update-vars-lst vars-lst (get-lambda-params exp) (+ current-major 1))))
                               (cond
                                 ((equal? (car exp) 'lambda-opt)
                                   (cons (car exp) (list (cadr exp) (caddr exp) (annotate-lexical-debug (get-body-3 exp) new-vars-lst (+ current-major 1)))))
                                 (else
                                   (cons (car exp) (list (get-lambda-params exp) (annotate-lexical-debug (get-body-3 exp) new-vars-lst (+ current-major 1)))))))))
                                 
                                          
            ((equal? (car exp)'var) (begin (display "case3 ") (display "exp is:") (display exp) (display "\n")
                     (make-lexical-var exp vars-lst current-major)))
            (else (begin (display "case else ") (display "exp is:") (display exp) (display "\n")
             (cons (annotate-lexical-debug (car exp) vars-lst current-major) (annotate-lexical-debug (cdr exp) vars-lst current-major))))
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


;; Annotating tail calls

(define tc-annotating-debug
  (lambda (expr tp?)
    (display "new exp:") (display expr) (display "\n")
    (display "tp is:") (display tp?) (display "\n")
    (cond
      ((equal? expr '()) (begin (display "case1 ") '()))
      ((or (const-3? expr) (var-p3? expr) (special-var? expr)) (begin (display "case2 ") (display "current exp is:") (display expr) (display "\n")
                                                                      expr)) 
      ((or-3? expr)  (begin (display "case 3 ") (display "current exp is:") (display expr) (display "\n")
                            `(or ,(append (map (lambda (item) (tc-annotating-debug item #f))  (get-all-but-last-elem (cadr expr)))  (list (tc-annotating-debug (get-last-elem (cadr expr)) tp?)) ))))
;      ((seq-3? expr) (begin (display "case4 ") (display "current exp is:") (display expr) (display "\n")
;                            `(seq ,(map (lambda (item) (tc-annotating-debug item tp?)) (cadr expr)))))
      ((seq-3? expr) (begin (display "case4 ") (display "current exp is:") (display expr) (display "\n")
                            `(seq ,(append (map (lambda (item) (tc-annotating-debug item #f))  (get-all-but-last-elem (cadr expr)))  (list (tc-annotating-debug (get-last-elem (cadr expr)) tp?)) ))))
      ((if-3? expr) (begin (display "case5 ") (display "current exp is:") (display expr) (display "\n")
                           `( ,(car expr) ,(tc-annotating-debug (cadr expr) #f) ,(tc-annotating-debug (caddr expr) tp?) ,(tc-annotating-debug (cadddr expr) tp?))))
      ((define-3? expr) (begin (display "case6 ") (display "current exp is:") (display expr) (display "\n")
                               `(def ,(cadr expr) ,(tc-annotating-debug (caddr expr) #f))))
      ((set-3? expr)  (begin (display "case7 ") (display "current exp is:") (display expr) (display "\n")
                             `(set ,(cadr expr) ,(tc-annotating-debug (caddr expr) #f))))
      ((lambda-3? expr) (begin (display "case8 ") (display "current exp is:") (display expr) (display "\n")
                               (if (equal? (car expr) 'lambda-opt)
                                   `(,(car expr) ,(cadr expr) ,(caddr expr) ,(tc-annotating-debug (get-body-3 expr) #t))
                                   `(,(car expr) ,(get-lambda-params-7 expr) ,(tc-annotating-debug (get-body-3 expr) #t)))))
      ((applic-3? expr) (if (equal? tp? #t)
                          (begin (display "case9.1 ") (display "current exp is:") (display expr) (display "\n")
                                 `(tc-applic ,(tc-annotating-debug (cadr expr) #f) ,(map (lambda (item) (tc-annotating-debug item #f)) (caddr expr))))
                           (begin (display "case9.2 ") (display "current exp is:") (display expr) (display "\n")
                                  `(applic ,(tc-annotating-debug (cadr expr) #f) ,(map (lambda (item) (tc-annotating-debug item #f)) (caddr expr))))))
      ((box-set? expr) `(box-set ,(cadr expr) ,(tc-annotating-debug (caddr expr) #f)))
    
      (else (begin (display "case else\n") (map (lambda (item) (if (list? item) (tc-annotating-debug item #f) item)) expr)))
      )))


(define tc-annotating
  (lambda (expr tp?)
    (cond
      ((equal? expr '())  '())
      ((or (const-3? expr) (var-p3? expr) (special-var? expr))  expr)
      ((or-3? expr)  `(or ,(append (map (lambda (item) (tc-annotating item #f))  (get-all-but-last-elem (cadr expr)))  (list (tc-annotating (get-last-elem (cadr expr)) tp?)) )))
      ;((seq-3? expr) `(seq ,(map (lambda (item) (tc-annotating item tp?)) (cadr expr))))
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

;;--MAAYAN-END--;;

;;----ELI-----
;(define annotate-tc 
;(letrec ((helper-tc (lambda (expr tp?)
;
;	(cond  ((and (list? expr)(or (eq? (car expr) 'var) (eq? (car expr) 'const) (eq? (car expr) 'pvar) (eq? (car expr) 'fvar) (eq? (car expr) 'bvar)))       expr)
;		   ((applic? expr) (if tp? 
;			                     `(tc-applic ,(helper-tc (applic-first-child expr) #f) ,(map (lambda (lst) (helper-tc lst #f)) (applic-rest-children expr)))
;					`(applic ,(helper-tc (applic-first-child expr) #f) ,(map (lambda (lst) (helper-tc lst #f)) (applic-rest-children expr)))))
;			((or? expr)
;
;			`(or ( ,@(map (lambda (x) (helper-tc x #f)) (get-or-exprs (cadr expr))) ,(helper-tc (get-or-last-exp (cadr expr)) tp?))))
;			((if? expr) `(if3 ,(helper-tc (get-test-if expr) #f) ,(helper-tc (get-dit-if expr) tp?) ,(helper-tc (get-dif-if expr) tp?)))
;			((define? expr) `(def ,(get-var-define expr) ,(helper-tc (get-expr-define expr) #f)))
;			((lambda? expr) 
			;`(,(car expr) ,(get-param-lambda expr) ,(map (lambda (lst) (helper-tc lst #f)) (get-or-exprs (getBody expr))) 
			;,(helper-tc (get-or-last-exp (getBody expr)) #t))
;			(creat_lambda_new expr 
;			     (if (not (null? (get-or-exprs (getBody expr))))
;				 (append (map (lambda (lst) (helper-tc lst #f)) (get-or-exprs (getBody expr))) (helper-tc (get-or-last-exp (getBody expr)) #t))
;				 (helper-tc (get-or-last-exp (getBody expr)) #t))))
;			((seq? expr) `(seq ( ,@(map (lambda (x) (helper-tc x #f)) (get-or-exprs (cadr expr))) ,(helper-tc (get-or-last-exp (cadr expr)) tp?))))
;			((set? expr) `(set ,(cadr expr) ,(helper-tc (caddr expr) #f))) 
;			((box-set? expr) `(box-set ,(cadr expr) ,(helper-tc (caddr expr) #f)))
;          
;			(else expr))
;		)) )
;		(lambda (expr)
;		 (helper-tc expr #f))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   
;//END-annotate-tc-adi//

