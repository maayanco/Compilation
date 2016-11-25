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

(define <sexpr-comment2>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr2>))
       (*caten 2)
       done))


(define <sexpr-comment>
  (new (*parser (word "#;"))
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
	(new 
		(*parser <CharPrefix>)

		(*parser <NamedChar>)
		(*parser <HexUnicodeChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)

		(*caten 2)
		(*pack-with (lambda (a s) `( ,@s) ))
		done))


;Should be case sensitive
;;should return string? currently returns chars..
(define <StringLiteralChar> 
	(new 
		(*parser <any-char>)
		(*parser (char #\\))
		;(*pack (lambda (a) (string a)  ))
		*diff
		done))

;(define <string-meta-char>
 ; (new (*parser (word "\\\\"))
  ;     (*pack (lambda (_) #\\))

  ;     (*parser (word "\\\""))
  ;     (*pack (lambda (_) #\"));

  ;     (*disj 2)
  ;     done))

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





;(define <String>
;	(^<skipped*>
;	(new
;		(*parser (word "\""))
;		(*parser <StringChar>) 
;		(*guard (lambda (a) 
;			(and (not (equal? a #\\)) (not (equal? a #\")) )))
;		*star
;		(*parser (word "\""))
;		(*caten 3)
;		(*pack-with (lambda (pre lst post) (fold-left string-append "" lst) )  )

;		done)))

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
	(new 
		(*parser <SymbolChar>) *plus
		(*pack (lambda (s) (string->symbol (list->string s))))
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

;(define <Number2>
;	(^<skipped*>
;		(new 
;			(*parser <NumNum>)
;			(*parser (range #\a #\z))
;			(*parser (range #\A #\Z))
;			(*disj 2)
;			*not-followed-by
;			done)))


(define <SpecialSymbol>
	(new
		(*parser <Symbol>)
		(*parser (char #\[))
		(*parser (char #\]))
		(*parser (char #\())
		(*parser (char #\())
		(*parser (char #\space))
	(*disj 5)
	*diff
	done))

(define <SymbolNotNumber>
  (new (*parser <Symbol>)
       (*parser (range #\0 #\9))
       *diff
       done))



(define <OnlyNumbers>
        (new 
                (*parser (not-followed-by <Number> <SymbolNotNumber>))
                done))


(define <ProperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr2>) ) *star
          ; (*pack (lambda (a) (display a) a))
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) s ))
		done))


(define <ImproperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr2>) ) *plus
		(*parser (word "."))
		(*delayed (lambda () <sexpr2>) )
		(*parser (word ")"))
		(*caten 5)
		(*pack-with (lambda (brk1 exp1 point exp2 brk2) `(,@exp1 . ,exp2) ))
		done))

(define <Vector>
	(new
		(*parser (word "#("))
		(*delayed (lambda () <sexpr2>) )
           ;(*pack (lambda (a) (symbol->char a)))
           *star
           (*pack (lambda (a) a))
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) (list->vector s)  ))
		done))

(define <Quoted>
	(new
		(*parser (word "'"))
		(*delayed (lambda () <sexpr2>) )
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quote s)))
		done))

(define <QuasiQuoted>
	(new
		(*parser (word "`"))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quasiquote s)))
		done))

(define <Unquoted>
	(new
		(*parser (word ","))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote s)))
		done))

(define <UnquoteAndSpliced>
	(new
		(*parser (word ",@"))
		(*delayed (lambda () <sexpr2>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote-splicing s) ))
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
		(*pack (lambda (s) (string->symbol (list->string s))))
		
	done)))

(define <InfixSymbolA>
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
		(^<skipped-infix*>
	(new	 
		 (*delayed (lambda () <InfixSexprEscape>))
		 (*delayed (lambda () <InfixFuncall>))
		 (*delayed (lambda () <InfixArrayGet>))
		 (*delayed (lambda () <InfixParen>))
		 
		 (*parser <Number>)
		 ;(*pack (lambda (a) (display "iii") a))
		 (*delayed (lambda () <InfixNeg>))
		 (*parser <InfixSymbol>)
		 ;(*pack (lambda (a) (display "i parsed") (display a) a))
		 (*guard (lambda (a) (and (not (eq? a "(")) (not (eq? a ")")) (not (eq? a "]")) (not (eq? a "[")) )))
		 
		 (*disj 7)
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
(^<skipped-infix*>
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
		(^<skipped-infix*>
	(new (*parser (word "-"))
		 (*delayed (lambda () <BasicExpression>) )
		 (*caten 2)
		 (*pack-with (lambda (minus exp) `(- ,exp)))
	done)))

(define <InfixMultOrDiv>
		(^<skipped-infix*>
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
		 (*pack-with (lambda (b c) (lambda (a) `(expt ,a ,c)))) *star
		 (*caten 2)
		 (*pack-with (lambda (a lambda_lst) 
			(fold-left (lambda (acc elment) (elment acc)) a lambda_lst)))
		 ;(*pack-with (lambda (exp1 sign exp2) `(expt ,exp1 ,exp2) ))
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
		(*delayed (lambda () <Number>))
		(*delayed (lambda () <InfixSymbol>))
		(*delayed (lambda () <InfixParen>))
		(*disj 3)
		;(*delayed (lambda () <BasicExpression>))
                (*parser (word "("))
		(*parser <whitespace>) *star
		(*parser (word ")"))
                (*caten 3)
                (*pack-with (lambda (brk1 exp2 brk2) (lambda (exp1) (cons exp1 '())))) *plus
		(*parser (word "("))
		(*delayed (lambda () <InfixArgList>))
		(*parser (word ")"))
		(*caten 3)
                (*pack-with (lambda (brk1 exp2 brk2) (lambda (exp1) (cons exp1 exp2) ))) *plus
                (*disj 2)
		(*caten 2)
		(*pack-with (lambda (exp1 lambdaExp) (fold-left (lambda (acc elment) (elment acc)) exp1 lambdaExp)))
		
		;(*parser <end-of-input>)
		;(*caten 2)
		;(*pack-with (lambda (a b) a ))
    done)))

(define <InfixFuncallOld> 
		(^<skipped-infix*>
	(new 
		(*delayed (lambda () <Number>))
           (*delayed (lambda () <InfixParen>))
           (*delayed (lambda () <InfixSymbol>))
		
		(*disj 3)
		(*parser <skip-infix>) *star
		;(*delayed (lambda () <BasicExpression>))
		(*parser (word "("))
		(*delayed (lambda () <InfixArgList>)) 
		(*parser (word ")"))
		(*parser <skip-infix>) *star
		(*caten 5)
		(*pack-with (lambda (a brk1 exp2 brk2 b) (lambda (exp1) (cons exp1 exp2) ))) *plus
		(*caten 2)
		(*pack-with (lambda (exp1 lambdaExp) (fold-left (lambda (acc elment) (elment acc)) exp1 lambdaExp)))
		
		;(*parser <end-of-input>)
		;(*caten 2)
		;(*pack-with (lambda (a b) a ))
    done)))

(define <InfixArrayGet> 
		(^<skipped-infix*>
	(new 
		(*delayed (lambda () <Number>))
           (*delayed (lambda () <InfixParen>))
           (*delayed (lambda () <InfixSymbol>))
		(*disj 3)
		;(*delayed (lambda () <BasicExpression>))
		(*parser <whitespace>) *star
		(*parser (word "["))
		(*delayed (lambda () <InfixAddOrSub>))
		(*parser (word "]"))
		(*parser <whitespace>) *star
		(*caten 5)
		(*pack-with (lambda (a brk1 exp2 brk2 b) (lambda (exp1) (list 'vector-ref exp1 exp2) ))) *plus
		(*caten 2)
		(*pack-with (lambda (exp1 lambdaExp) (fold-left (lambda (acc elment) (elment acc)) exp1 lambdaExp)))
		;(*pack-with (lambda (exp1 brk1 exp2 brk2) (list 'vector-ref exp1 exp2) ))
		;(*parser <end-of-input>)
		;(*caten 2)
		;(*pack-with (lambda (a b) a ))
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
		 (*delayed (lambda () <sexpr2>))
		 (*caten 2)
		 (*pack-with (lambda (prefix exp) exp ))
	done)))


(define <InfixExpression>
(^<skipped-infix*>
	(new
		;(*parser <InfixArrayGet>)
		;(*pack (lambda (a) (display "InfixArrayGet\n") a ))

		;(*parser <InfixFuncall>)
		;(*pack (lambda (a) (display "InfixFuncall\n") a))

		(*parser <InfixAddOrSub>)
		;(*pack (lambda (a) (display "InfixAddOrSub") a ))
		;(*pack (lambda (a) (display "InfixAddOrSub\n") a))
		(*parser <InfixSexprEscape>)
		;(*pack (lambda (a) (display "InfixSexprEscape") a ))
		;(*parser <InfixParen>)
		;(*pack (lambda (a) (display "InfixParen\n") a))

		;(*parser <InfixNeg>)
		;(*pack (lambda (a) (display "InfixNeg\n") a))

		;(*parser <InfixSexprEscape>)
		;(*pack (lambda (a) (display "InfixSexprEscape\n") a))

		(*disj 2)
		done)))

(define <InfixExtension>
	(^<skipped-infix*>
	(new (*parser <InfixPrefixExtensionPrefix>)
		 (*parser <InfixExpression>)
		 (*caten 2)
		 (*pack-with (lambda (sign exp) exp))
		 done)))


(define <sexpr2>
	(^<skipped*>
	(new
		(*parser <InfixExtension>)
          ; (*pack (lambda (a) (display "<InfixExtension>") a))
		(*parser <ImproperList>)
         ;  (*pack (lambda (a) (display "<ImproperList>") a))
		(*parser <ProperList>)
   ;        (*pack (lambda (a) (display "<ProperList>") a))
		(*parser <Vector>)
    ;       (*pack (lambda (a) (display "<Vector>") a))
		(*parser <Boolean>)
     ;      (*pack (lambda (a) (display "<Boolean>") a))
		(*parser <Quoted>)
      ;     (*pack (lambda (a) (display "<Quoted>") a))
		(*parser <QuasiQuoted>)
       ;    (*pack (lambda (a) (display "<QuasiQuoted>") a))
		(*parser <Unquoted>)
        ;   (*pack (lambda (a) (display "<Unquoted>") a))
		(*parser <UnquoteAndSpliced>)
         ;  (*pack (lambda (a) (display "<UnquoteAndSpliced>") a))
		
		;(*parser <Number>)
		
		(*parser <OnlyNumbers>)
          ; (*pack (lambda (a) (display "<OnlyNumbers>") a))
		(*parser <Char>)
           ;(*pack (lambda (a) (display "<Char>") a))
		(*parser <Symbol>)
           ;(*pack (lambda (a) (display "s<Symbol>ymbol") a))
		(*parser <String>)
          ; (*pack (lambda (a) (display "<String>") a))
		;(*disj 12)
		(*disj 13)
		done)))
		
