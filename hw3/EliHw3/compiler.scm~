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
							
(define eliminate-nested-defines
	(lambda (expr)
		(cond ((lambda? expr) 
			(foo (getBody expr) 
			   (lambda (ds es) 
					(if (null? ds) 
						(creat_lambda_new expr (car (map eliminate-nested-defines (getBody expr))))
						(let*  ((setters (map (lambda (ds)`(set ,(cadr ds) ,(eliminate-nested-defines (caddr ds)))) ds))
						       (vars (map (lambda (ds) (cadadr ds)) ds))
							   (es (map eliminate-nested-defines es))
							   (bodyOfLambdaSimple `(seq ,(append setters es)))
							   (lambdaApplic `(lambda-simple ,vars ,bodyOfLambdaSimple))
							   (eliminate-lambdaApplic  lambdaApplic)
							   (falseVar (map (lambda (vars) '(const #f))  vars))
							   (creatTheApplic `(applic ,eliminate-lambdaApplic ,falseVar)))

                         (creat_lambda_new expr creatTheApplic))))))
			 ((list? expr) (map eliminate-nested-defines expr))
			 (else expr))))
									
		   
			   
;//MEYAR-FUNCTION-FROM-CLASS//

(define foo
    (lambda (pes ret-ds+es)
        (if (null? pes) (ret-ds+es '() '())
            (foo (cdr pes)
                (lambda(ds es)
                    (cond ((eq? (caar pes) 'def)
                            (ret-ds+es (cons (car pes) ds) es))
                            ((eq? (caar pes) 'seq)
                            (foo (cadar pes)
                              (lambda(ds1 es1)
                              (ret-ds+es (append ds1 ds)
                                        (append es1 es)))))
                            (else (ret-ds+es ds (cons (car pes) es)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 3 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remove-applic-lambda-nil
(let    ((run 
			(compose-patterns
				(pattern-rule
					`(applic (lambda-simple() ,(? 'expr) ) () )
					(lambda(expr) (remove-applic-lambda-nil expr))))))
	(lambda (expr)

		(run expr 
		(lambda ()  (if (list? expr)
							(map remove-applic-lambda-nil expr)
							expr))))))
							
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 4 END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

							
(define is-arg-bound?
    (lambda (arg body)
        (cond 
            ((null? body) #f)
            ((not(list? body)) #f)
            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f)
            ((and (is-lambda? body) (not(arg-in-param arg (get-params body)))) 
                                                            (is-arg-really-bound? arg (car(cdr(cdr body)))))
            (else (ormap (lambda(x)(is-arg-bound? arg x)) body) )
        )
))

;looking for appearance of "(var arg)" (which is not inside a problematic lambda)
(define is-arg-really-bound? 
    (lambda (arg body)
        (cond 
            ((null? body) #f)
            ((not(list? body)) #f)
            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f)
            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg)) #t)
            (else 
             (ormap (lambda(x)(is-arg-really-bound? arg x)) body)
            )
        )
))

(define arg-is-SET? 
    (lambda (arg body)
        (cond 
            ((null? body) #f)
            ((not(list? body)) #f)
            ((and (is-lambda? body) (arg-in-param arg (get-params body))) #f) ;; need to lehatim also to lambda opt and var
            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) #t)
            (else 
             (ormap (lambda(x)(arg-is-SET? arg x)) body)
            )
        )
))

(define arg-is-GET? 
    (lambda (arg body)
        (cond 
            ((null? body) #f)
            ((not(list? body)) #f)
            ((and (is-lambda? body) (arg-in-param arg (get-params body)) #f)) ;; need to lehatim also to lambda opt and var
            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
                (arg-is-GET? arg (cdr(cdr body)))) 
            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg)) #t) ; maybe
            (else 
             (ormap (lambda(x)(arg-is-GET? arg x)) body)
            )
        )
))

(define get-params
	(lambda (exp)
        (let*(
            (type-of-lambda (car exp))
            (args (cond 
                    ((equal? type-of-lambda 'lambda-simple) (car (cdr exp)))
                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
                    ((equal? type-of-lambda 'lambda-opt) `( ,@(car(cdr exp)) ,(car(cdr(cdr exp))))    )
                    
                    ))
            
            )
        args))
	)

(define get-body
	(lambda (exp) 
        (let*(
            (type-of-lambda (car exp))
            
            (body (cond 
                    ((equal? type-of-lambda 'lambda-simple) (car(cdr(cdr exp))))
                    ((equal? type-of-lambda 'lambda-var) (car(cdr(cdr exp))))
                    ((equal? type-of-lambda 'lambda-opt) (car(cdr(cdr(cdr exp)))))
                    ))
            )
        body))

	)


(define arg-in-param
    (lambda (arg args)
        (if (member arg args) 
            #t
            #f
)))

                                    
                                    
(define smallest? 
       (lambda(exp)
       (not(ormap (lambda(x)(and(list? x)(not(quote? x)))) exp))))
      
      


(define is-lambda?
    (lambda (exp)
        (or (lambda-simple-exp? exp)(lambda-opt-exp? exp)(lambda-variadic-exp? exp))
        ))
   


(define lambda-simple-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-simple))))
    
(define lambda-opt-exp?
  (lambda(exp) 
    (and (pair? exp) (equal? (car exp) 'lambda-opt))))

(define lambda-variadic-exp?
  (lambda(exp)
    (and (pair? exp) (equal? (car exp) 'lambda-var))))
	

	
	
(define box-set
    (lambda (exp)
        (cond 
            ((null? exp) exp)
            ((not(list? exp)) exp)           
            ((is-lambda? exp) (lambda-helper exp))
            ((null?(cdr exp)) (list(box-set (car exp)))) 
            ((null?(car exp)) (list(box-set (cdr exp))))
            ;; need to fix!!! what Elad said was true.. there are more cases to check. Elad sent me in facebook some reference
            (else 
            ;(map (lambda(x)(box-set exp)) exp)
            (cons (box-set (car exp)) (box-set (cdr exp)))
            ;(map (box-set (car exp)) (box-set (cdr exp))))
        )   
)))



; get list of args and body
; returns: body updated
(define lambda-and-params-helper
    (lambda (params body)
        (cond 
            ((null? params) body)
            ((not(list? params)) body)
            ((=(length params)0) body)
            ((=(length params)1)
            ; (begin (display params) (display 'asdasdasd)
             	(box-arg-if-needed (car params) body))
             ;)
            (else 
            (lambda-and-params-helper (cdr params) (box-arg-if-needed (car params) body))
            )
        )   
))

;get lambda-exp
;NEED TO FIX THE ACCESS TO ARGS IN LAMBDA OPT AND VAR
(define lambda-helper
    (lambda (exp)
        (let*(
            (type-of-lambda (car exp))
            (original-args
            	(cond 
                    ((equal? type-of-lambda 'lambda-simple) (list(car (cdr exp))))
                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
                    ((equal? type-of-lambda 'lambda-opt) (list (car(cdr exp)) (car(cdr(cdr exp)))))
                    )
            	)
            (args (cond 
                    ((equal? type-of-lambda 'lambda-simple) (car (cdr exp)))
                    ((equal? type-of-lambda 'lambda-var) (list(car (cdr exp))))
                    ((equal? type-of-lambda 'lambda-opt) `( ,@(car(cdr exp)) ,(car(cdr(cdr exp))))    )
                    ))
            (args-final
                (reverse args)
            )
            (body (cond 
                    ((equal? type-of-lambda 'lambda-simple) (car(cdr(cdr exp))))
                    ((equal? type-of-lambda 'lambda-var) (car(cdr(cdr exp))))
                    ((equal? type-of-lambda 'lambda-opt) (car(cdr(cdr(cdr exp)))))
                    ))
            (body-final
                (box-set body)
                )
            )
            (if(= 0(length args))
            `(,type-of-lambda,@original-args, body-final)
            ;(begin (display 'type: ) (display type-of-lambda)(display ' agrs: ) (display args) 
            	`(,type-of-lambda,@original-args, (lambda-and-params-helper args-final body-final))
            ;	)
            )      
)))


(define box-arg-if-needed
    (lambda (arg body)
         (if
         (and(arg-is-GET? arg body)(is-arg-bound? arg body)(arg-is-SET? arg body))
         (make-changes arg body)
         body
         )
))

(define make-changes 
    (lambda (arg body)
        (make-changes-add-expression arg (make-changes-set arg (make-changes-get arg body)))
    )
)



(define make-changes-add-expression
    (lambda (arg body)
      (let(
        (header `(set, (list 'var arg), (list 'box (list 'var arg))))
        )
        (if(equal? (car body) 'seq)
            `(seq,`(,header,@(car(cdr body))))
            `(seq, `(,header, body))
    ))))

(define make-changes-set
    (lambda (arg body)
         (cond 
            ((null? body) body)
            ((not(list? body)) body)
            ((and (is-lambda? body) (arg-in-param arg (get-params body))) body) ;; need to lehatim also to lambda opt and var
            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
                                                                                            `(box-set, (car(cdr body)), (car(cdr(cdr body))) ))
            (else 
             (cons (make-changes-set arg (car body)) (make-changes-set arg (cdr body)))
            )
        )
))





(define make-changes-get
    (lambda (arg body)
         (cond 
            ((null? body) body)
            ((not(list? body)) body)
            ((and (is-lambda? body) (arg-in-param arg (get-params body))) body) ;; need to lehatim also to lambda opt and var
            ((and (= 3(length body)) (equal? (car body) 'set) (equal? (car(car(cdr body))) 'var) (equal? (car(cdr(car(cdr body)))) arg)) 
                `(set, (list 'var arg),@(make-changes-get arg (cdr(cdr body))))) 
            ((and (smallest? body) (= 2(length body)) (equal? (car body) 'var) (equal? (car(cdr body)) arg))
                                                                                            `(box-get, (list 'var arg) ))
            (else 
             (cons (make-changes-get arg (car body)) (make-changes-get arg (cdr body)))
            )
        )
))


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


               
(define pe->lex-pe
                    (letrec ((helper-pe (lambda (expr lst) 
                    
					(if (and (lambda? expr)(lambda-simple? expr))
                        (let ((vars (getLambdaSimPleVars expr))
                             (body (get-body expr)))
							
                             (creat_lambda_new expr  (helper-pe body (cons vars lst))))
					(if (and (lambda? expr)(lambda-var? expr))
                        (let ((vars (getLambdaVarVars expr))
                             (body (get-body expr)))
							 
                             (creat_lambda_new expr (helper-pe body (cons vars lst))))
				    (if (and (lambda? expr)(lambda-opt? expr))
                        (let ((vars (getLambdaOptVars expr))
                             (body (get-body expr)))
							 
                             (creat_lambda_new expr  (helper-pe body (cons vars lst))))
					(if (applic? expr)
                    `(applic ,(helper-pe (applic-first-child expr) lst) ,(map (lambda(x) (helper-pe x lst) ) (applic-rest-children expr)))
                    (if (and (list? expr) (not (null? expr)))
                        (if (eq? 'var (car expr))
                            (serch (cadr expr) lst)
                            (map (lambda(x) (helper-pe x lst)) expr)) expr))))))))
                            (lambda (sexpr)
				(helper-pe sexpr '() ))          
                            ))

			

;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 6 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define annotate-tc 
(letrec ((helper-tc (lambda (expr tp?)

	(cond  ((and (list? expr)(or (eq? (car expr) 'var) (eq? (car expr) 'const) (eq? (car expr) 'pvar) (eq? (car expr) 'fvar) (eq? (car expr) 'bvar)))       expr)
		   ((applic? expr) (if tp? 
			                     `(tc-applic ,(helper-tc (applic-first-child expr) #f) ,(map (lambda (lst) (helper-tc lst #f)) (applic-rest-children expr)))
					`(applic ,(helper-tc (applic-first-child expr) #f) ,(map (lambda (lst) (helper-tc lst #f)) (applic-rest-children expr)))))
			((or? expr)

			`(or ( ,@(map (lambda (x) (helper-tc x #f)) (get-or-exprs (cadr expr))) ,(helper-tc (get-or-last-exp (cadr expr)) tp?))))
			((if? expr) `(if3 ,(helper-tc (get-test-if expr) #f) ,(helper-tc (get-dit-if expr) tp?) ,(helper-tc (get-dif-if expr) tp?)))
			((define? expr) `(def ,(get-var-define expr) ,(helper-tc (get-expr-define expr) #f)))
			((lambda? expr) 
			;`(,(car expr) ,(get-param-lambda expr) ,(map (lambda (lst) (helper-tc lst #f)) (get-or-exprs (getBody expr))) 
			;,(helper-tc (get-or-last-exp (getBody expr)) #t))
			(creat_lambda_new expr 
			     (if (not (null? (get-or-exprs (getBody expr))))
				 (append (map (lambda (lst) (helper-tc lst #f)) (get-or-exprs (getBody expr))) (helper-tc (get-or-last-exp (getBody expr)) #t))
				 (helper-tc (get-or-last-exp (getBody expr)) #t))))
			((seq? expr) `(seq ( ,@(map (lambda (x) (helper-tc x #f)) (get-or-exprs (cadr expr))) ,(helper-tc (get-or-last-exp (cadr expr)) tp?))))
			((set? expr) `(set ,(cadr expr) ,(helper-tc (caddr expr) #f))) 
			((box-set? expr) `(box-set ,(cadr expr) ,(helper-tc (caddr expr) #f)))
           
			(else expr))
		)) )
		(lambda (expr)
		 (helper-tc expr #f))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; TASK 7 END;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   
;//END-annotate-tc-adi//

