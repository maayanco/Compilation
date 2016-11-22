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
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr2>))
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


(define <digit-0-9> (range #\0 #\9))
(define <digit-1-9> (range #\1 #\9))

(define <Boolean> 
	(^<skipped*>
	(new
		(*parser (word-ci "#t"))
		(*pack (lambda (_) #t))
		(*parser (word-ci "#f"))
		(*pack (lambda (_) #f))
		(*disj 2)
		done)))


(define <CharPrefix> 
	(new	(*parser (char #\#))
			(*parser (char #\\)) 
		    (*caten 2)
		    (*pack-with (lambda (a b) #\\))
		done))


; Should be case sensitive
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

;;Question: should A to Z be included?
;;Another Question: should the input be a string or a char?
;; currently we get "a" not "#\\a"
(define <HexChar>
	(new (*parser (range #\0 #\9) )
		(*parser (range #\a #\f) )
		(*disj 2)
	done))


;; should i accept only x or also capital?
(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*pack (lambda (_) `#\x))
		(*parser (char #\X))
		(*pack (lambda (_) `#\x))
		(*disj 2)
		(*parser <HexChar>) *plus
		(*caten 2)
		(*pack-with (lambda (a s) (integer->char (string->number (list->string `(,@s) )  16) )))
		done))


(define <Char>
	(^<skipped*>
	(new 
		(*parser <CharPrefix>)

		(*parser <NamedChar>)
		(*parser <HexUnicodeChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)

		(*caten 2)
		(*pack-with (lambda (a s) `( ,@s) ))
		done)))


;Should be case sensitive
;;should return string? currently returns chars..
(define <StringLiteralChar> 
	(new 
		(*parser <any-char>)
		(*parser (char #\\))
		*diff
		done))


(define <StringMetaChar> 
	(new
		(*parser (word-ci "\\"))
		(*pack (lambda (_) #\\))

		(*parser (word-ci "\""))
		(*pack (lambda (_) #\"))

		(*parser (word-ci "\t"))
		(*pack (lambda (_) #\t))

		(*parser (word-ci "\f"))
		(*pack (lambda (_) #\f))

		(*parser (word-ci "\n"))
		(*pack (lambda (_) #\n))

		(*parser (word-ci "\r"))
		(*pack (lambda (_) #\r))

		(*disj 6)
		done))


;; i added an if inside the lambda, is it ok?
;; why not really
(define <StringHexChar>
	(new 
		(*parser (word "\\"))
		(*parser (word-ci "x"))
		(*parser <HexChar>) *star
		(*parser (word ";"))
		(*caten 4)
		(*pack-with (lambda (pre x s comma) 
			(if (null? s)
			""
			(integer->char (string->number (list->string `(,@s) )  16) ))))
		done))


(define <StringChar>
	(new 
		(*parser <StringMetaChar>)
		(*parser <StringHexChar>)
		(*parser <StringLiteralChar>)
		(*disj 3)
		done))



(define <String>
	(^<skipped*>
	(new
		(*parser (word "\""))
		(*parser <StringChar>) 
		(*guard (lambda (a) (not (equal? a #\"))  ))
		*star
		(*parser (word "\""))
		(*caten 3)
		(*pack-with (lambda (pre lst post) (list->string `(,@lst))  ))
		done)))


;; returns chars, is it ok??
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
		;(*pack (lambda (a) (string->symbol (list->string (list a)))))
	;	(*pack (lambda (a) (list->quote `(,@a))))
	done))


(define <Symbol>
	(^<skipped*>
	(new 
		(*parser <SymbolChar>) *plus
		(*pack (lambda (s) (string->symbol (list->string s))))
		done)))
		
(define <Natural>
	;(^<skipped*>
	(new
		(*parser <digit-0-9>) *plus
		(*pack (lambda (a) (string->number (list->string a)) ))
		done))
;)

(define <Integer>
;	(^<skipped*>
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
;)
       
 (define <Fraction>
 	;(^<skipped*>
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
	(^<skipped*>
    (new 
         (*parser <Fraction>)
         (*parser <Integer>)
         (*disj 2)
         done)))

(define <OnlyNumbers>
	(^<skipped*>
        (new 
                (*parser (not-followed-by <Number> <Symbol>))
                done)))


(define <ProperList>
	(^<skipped*>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr2>) ) *star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) s ))
		done)))


(define <ImproperList>
	(^<skipped*>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr2>) ) *plus
		(*parser (word "."))
		(*delayed (lambda () <sexpr2>) )
		(*parser (word ")"))
		(*caten 5)
		(*pack-with (lambda (brk1 exp1 point exp2 brk2) `(,@exp1 . ,exp2) ))
		done)))

(define <Vector>
	(^<skipped*>
	(new
		(*parser (word "#("))
		(*delayed (lambda () <sexpr2>) ) *star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) (list->vector s)  ))
		done)))

(define <Quoted>
	(^<skipped*>
	(new
		(*parser (word "'"))
		(*delayed (lambda () <sexpr2>) )
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quote s)))
		done)))

(define <QuasiQuoted>
	(^<skipped*>
	(new
		(*parser (word "`"))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quasiquote s)))
		done)))

(define <Unquoted>
	(^<skipped*>
	(new
		(*parser (word ","))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote s)))
		done)))

(define <UnquoteAndSpliced>
	(^<skipped*>
	(new
		(*parser (word ",@"))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote-splicing s) ))
		done)))


;; helper parser - returns the type of parsed expression, instead of the parsed value
(define <sexprh>
	(new
		(*parser <ImproperList>)
		(*pack (lambda (_) `improperlist))

		(*parser <ProperList>)
		(*pack (lambda (_) `ProperList))

		(*parser <Vector>) ;should be packed
		(*pack (lambda (_) `Vector))

		(*parser <Boolean>)
		(*pack (lambda (_) `Boolean))

		(*parser <Quoted>)
		(*pack (lambda (_) `Quoted))

		(*parser <QuasiQuoted>)
		(*pack (lambda (_) `QuasiQuoted))

		(*parser <Unquoted>)
		(*pack (lambda (_) `Unquoted))

		(*parser <UnquoteAndSpliced>)
		(*pack (lambda (_) `UnquoteAndSpliced))

		(*parser <Number>)
		(*pack (lambda (_) `Number))

		(*parser <Char>)
		(*pack (lambda (_) `Char))

		(*parser <Symbol>)
		(*pack (lambda (_) `Symbol))

		(*parser <String>)
		(*pack (lambda (_) `String))

		(*disj 12)
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
		;(*parser (char #\**))
		(*parser (char #\^))
		(*parser (char #\/))
	(*disj 9)
	done))

(define <InfixSymbol> 
	(^<skipped*>
	(new 
		(*parser <SymbolChar>)
		(*pack (lambda (a) `(,@a)))
		(*parser <InfixUndesirables>)
		*diff
		*plus
		(*pack (lambda (s) (string->symbol (list->string s))))
		
	done)))

;(define <InfixSymbol2>
;	(new (*parser <Symbol>)
;		 (*parser <)
;		 (*guard differentThan)
;		 (*pack (lambda (symb) (string->list (symbol->string symb)) ))
;		 (*guard (lambda (lst) (not (contains lst #\+)) ))
		 
		 ;(*guard (lambda (exp) (andmap  (lambda (item) (equal? item #\+))  (string->list (symbol->string exp)) )))
		;(*guard (lambda (a) (display (symbol? a)) (display (string->list (symbol->string a))) (andmap (lambda (a) (display a) (not (eq? a (string->symbol "^")))) (string->list (symbol->string a)) ) ))
		 ;(*guard (lambda (a) (display a) 
		 ;						(not (or 
		  ;							(equal? a (string->symbol "+"))
		  ;							(equal? a (string->symbol "-"))
		  ;							(equal? a (string->symbol "**"))
		  ;							(equal? a (string->symbol "*"))
		  ;						    (equal? a (string->symbol "^")) 
		  ;							(equal? a (string->symbol "/")))
		 
		; (*pack (lambda (a) (display (symbol? a)) a ))
		 ; (*parser (word "["))
		 ; *not-followed-by
;		done))

;(define <sexpr>
;  (^<skipped*>
;   (disj <boolean>
;	 )))

(define <BasicExpression>
	(^<skipped*>
	(new	 
		 
		 (*delayed (lambda () <InfixFuncall>))
		 (*delayed (lambda () <InfixArrayGet>))
		 (*delayed (lambda () <InfixParen>))
		 
		 (*parser <Number>)
		 (*delayed (lambda () <InfixNeg>))
		 (*parser <InfixSymbol>)
		 (*guard (lambda (a) (and (not (eq? a "(")) (not (eq? a ")")) (not (eq? a "]")) (not (eq? a "[")) )))
		 
		 (*disj 6)
	done)))

(define <InfixPrefixExtensionPrefix>
	(new (*parser (word "##"))
		 (*parser (word "#%"))
		 (*disj 2)
	done))

;; TODO: (define <InfixExpression> ...)


;(define <InfixAddOrSub>
;	(new 
;		(*delayed (lambda () <InfixMultOrDiv>))
;		(*parser (char #\+))
;		(*parser (char #\-))
;		(*disj 2)
;		(*delayed (lambda () <InfixMultOrDiv>))
;		(*caten 3)
		;(*pack-with (lambda (exp1 sign exp2) `(_ ,exp1 ,exp2) ))
;		done))

(define <InfixAddOrSub>
(^<skipped*>
(new (*delayed (lambda () <InfixMultOrDiv>))
     (*parser (char #\+))
	 (*parser (char #\-))
	 (*disj 2)
	 (*delayed (lambda () <InfixMultOrDiv>))
	 (*caten 2)
	 (*pack-with (lambda (sign c) (lambda (a) (list (string->symbol (string sign)) a c)))) *star
	 (*caten 2)
	 (*pack-with (lambda (a lambda_lst) 
		(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
done)))


(define <InfixNeg>
	(^<skipped*>
	(new (*parser (word "-"))
		 (*delayed (lambda () <InfixExpression>) )
		 (*caten 2)
		 (*pack-with (lambda (minus exp) `(- ,exp)))
	done)))

(define <InfixMultOrDiv>
	(^<skipped*>
	(new (*delayed (lambda () <InfixPow>))
		 (*parser (char #\*))
		 (*parser (char #\/))
		 (*disj 2)
		 (*delayed (lambda () <InfixPow>))
		 (*caten 2)
		 (*pack-with (lambda (b c) (lambda (a) (list (string->symbol (string b)) a c)))) *star
		 (*caten 2)
		 (*pack-with (lambda (a lambda_lst) 
			(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
	done)))



(define <PowerSymbol>
	(new (*parser (word "^"))
		 (*pack (lambda (a) (string->symbol "^")))
		 (*parser (word "**"))
		 (*pack (lambda (a) (string->symbol "**")))
		 (*disj 2)
	done))

(define <InfixPow>
	(^<skipped*>
	(new (*parser <BasicExpression>)
		 (*parser <PowerSymbol>)
		 (*delayed (lambda () <InfixPow>))
		 (*parser <BasicExpression>)
		 (*disj 2)
		 (*caten 2)
		 (*pack-with (lambda (b c) (lambda (a) `(expt ,a ,c)))) *star
		 (*caten 2)
		 (*pack-with (lambda (a lambda_lst) 
			(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
		 ;(*pack-with (lambda (exp1 sign exp2) `(expt ,exp1 ,exp2) ))
	done)))


(define <InfixArgList>
	(^<skipped*>
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
	(^<skipped*>
	(new 
		(*delayed (lambda () <Number>))
		(*delayed (lambda () <InfixSymbol>))
		(*delayed (lambda () <InfixParen>))
		(*disj 3)
		;(*delayed (lambda () <BasicExpression>))
		(*parser (word "("))
		(*delayed (lambda () <InfixArgList>)) 
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (brk1 exp2 brk2) (lambda (exp1) (cons exp1 exp2) ))) *plus
		(*caten 2)
		(*pack-with (lambda (exp1 lambdaExp) (fold-left (lambda (acc elment) (elment acc)) exp1 lambdaExp)))
		
		;(*parser <end-of-input>)
		;(*caten 2)
		;(*pack-with (lambda (a b) a ))
    done)))

(define <InfixArrayGet> 
	(^<skipped*>
	(new 
		(*delayed (lambda () <Number>))
		(*delayed (lambda () <InfixSymbol>))
		(*delayed (lambda () <InfixParen>))
		(*disj 3)
		;(*delayed (lambda () <BasicExpression>))
		(*parser (word "["))
		(*delayed (lambda () <InfixAddOrSub>))
		(*parser (word "]"))
		(*caten 3)
		(*pack-with (lambda (brk1 exp2 brk2) (lambda (exp1) (list 'vector-ref exp1 exp2) ))) *plus
		(*caten 2)
		(*pack-with (lambda (exp1 lambdaExp) (fold-left (lambda (acc elment) (elment acc)) exp1 lambdaExp)))
		;(*pack-with (lambda (exp1 brk1 exp2 brk2) (list 'vector-ref exp1 exp2) ))
		;(*parser <end-of-input>)
		;(*caten 2)
		;(*pack-with (lambda (a b) a ))
    done)))


(define <InfixParen>
	(^<skipped*>
	(new (*parser (word "("))
		 (*delayed (lambda () <InfixAddOrSub>))
		 (*parser (word ")"))
		 (*caten 3)
		 (*pack-with (lambda (brk1 exp brk2) exp ))
	done)))

(define <InfixSexprEscape>
	(^<skipped*>
	(new (*parser <InfixPrefixExtensionPrefix>)
		 (*delayed (lambda () <sexpr2>))
		 (*caten 2)
		 (*pack-with (lambda (prefix exp) exp ))
	done)))


(define <InfixExpression>
	(^<skipped*>
	(new
		;(*parser <InfixArrayGet>)
		;(*pack (lambda (a) (display "InfixArrayGet\n") a ))

		;(*parser <InfixFuncall>)
		;(*pack (lambda (a) (display "InfixFuncall\n") a))

		(*parser <InfixAddOrSub>)
		;(*pack (lambda (a) (display "InfixAddOrSub\n") a))

		;(*parser <InfixParen>)
		;(*pack (lambda (a) (display "InfixParen\n") a))

		;(*parser <InfixNeg>)
		;(*pack (lambda (a) (display "InfixNeg\n") a))

		;(*parser <InfixSexprEscape>)
		;(*pack (lambda (a) (display "InfixSexprEscape\n") a))

		;(*disj 2)
		done)))

(define <InfixExtension>
	(^<skipped*>
	(new (*parser <InfixPrefixExtensionPrefix>)
		 (*parser <InfixExpression>)
		 (*caten 2)
		 (*pack-with (lambda (sign exp) exp))
		 done)))


(define <sexpr2>
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
		
		;(*parser <Number>)
		
		
		(*parser <Number>)
		(*parser <Char>)
		(*parser <Symbol>)
		(*parser <String>)
		;(*disj 12)
		(*disj 13)
		done)))
		
