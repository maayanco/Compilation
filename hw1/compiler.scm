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
		
