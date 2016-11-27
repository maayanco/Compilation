(load "pc.scm")
 
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
       (*pack-with (lambda (_ rest) (if (and (>= (hexa->digit rest) 0) (< (hexa->digit rest)  1114112)) (integer->char (hexa->digit rest)) 'failed-to-convert)))
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

	   (*delayed (lambda () <sexpr2>))
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
     (*delayed (lambda () <sexpr2>))
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
     (*delayed (lambda () <sexpr2>) )
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda ( a b c) b))*plus
     (*parser (char #\.))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr2>) )
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
     (*delayed (lambda () <sexpr2>) )
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
     (*delayed (lambda () <sexpr2>))
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
     (*delayed (lambda () <sexpr2>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
     (*pack-with (lambda (a b) (list 'quasiquote  b)))
     done))
 
(define <UnquoteAndSpliced>
(new (*parser (word-ci ",@"))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr2>))
     (*parser <Skip>) *star
     (*caten 3)
     (*pack-with (lambda (a b c) b))
     (*caten 2)
     (*pack-with (lambda (a b) (list 'unquote-splicing b)))
     done))
 
(define <Unquoted>
(new (*parser (char #\,))
     (*parser <Skip>) *star
     (*delayed (lambda () <sexpr2>))
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
	 (*pack-with (lambda (a b)  (display a) (list (string->symbol(string a)) b)))
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
(new (*parser <Skip>) *star
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
done))

(define <InfixAdd> <InfixAddandSub>)
(define <InfixSub> <InfixAddandSub>)
(define <InfixDiv> <InfixMulandDiv>)
(define <InfixMul> <InfixMulandDiv>)

(define <InfixPow>
(new (*parser <Skip>) *star
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
done))


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
     (*delayed (lambda () <sexpr2>))
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





  
(define <sexpr2>
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
    (new (*parser <sexpr2>) *plus
    done))


       


       
