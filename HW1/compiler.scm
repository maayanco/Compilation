(include "pc.scm")


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
		done))


(define <VisibleSimpleChar> 
	(new (*parser (range #\space #\~)) *star
	done))

(define <NamedChar>
	(new
		(*parser (word "lambda"))
		(*pack (lambda (_) (integer->char 955)))
		(*parser (word "newline"))
		(*pack (lambda (_) `#\newline))
		(*parser (word "nul"))
		(*pack (lambda (_) `#\nul))
		(*parser (word "page"))
		(*pack (lambda (_) `#\page ))
		(*parser (word "return"))
		(*pack (lambda (_) `#\return ))
		(*parser (word "space"))
		(*pack (lambda (_) `#\space ))
		(*parser (word "tab"))
		(*pack (lambda (_) `#\tab ))
		(*disj 7)
		done))

(define <HexChar>
	(new (*parser (range #\0 #\9) )
		(*parser (range #\a #\f) )
		(*disj 2)
	done))


(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*pack (lambda (_) `#\x))
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


(define <StringLiteralChar> 
	(new 
		(*parser <any-char>)
		(*parser (char #\\))
		*diff
		done))


(define <StringMetaChar> 
	(new
		(*parser (word-ci "\\"))
		(*parser (word-ci "\""))
		(*parser (word-ci "\t"))
		(*parser (word-ci "\f"))
		(*parser (word-ci "\n"))
		(*parser (word-ci "\r"))
		(*disj 6)
		done))



(define <StringHexChar>
	(new 
		(*parser (char #\\))
		(*parser (char #\x))
		(*parser <HexChar> ) *star
		(*parser (char #\;))
		(*caten 4)
		done))


(define <StringChar>
	(new 
		(*parser <StringLiteralChar>)
		(*parser <StringMetaChar>)
		(*parser <StringHexChar>)
		(*disj 3)
		done))




(define <String>
	(new
		(*parser (char #\"))
		(*parser <StringChar>) *star
		(*parser (char #\"))
		(*caten 3)
		done))


(define <SymbolChar>
	(new 
		(*parser (range #\0 #\9))
		(*parser (range #\a #\z))
		(*parser (range #\A #\Z))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\<))
		(*parser (char #\>))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 14)
	done))
;ok

(define <Symbol>
	(new 
		(*parser <SymbolChar>) *plus
		;(*pack-with (lambda (s) (list? s) ))
		done))


(define <Sexpr>
	(new
		(*parser <Boolean>)
		(*parser <Char>)
		;(*parser <Number>)
		;(*paresr <String>)
		(*parser <Symbol>)
		(*parser <ProperList>)
		;(*parser <ImproperLIst>)
		;(*parser <Vector>)
		;(*parser <Quoted>)
		;(*parser <QuasiQuoted>)
		;(*parser <Unquoted>)
		;(*parser <UnquoteAndSpliced>)
		;(*parer <InfixExtension>)
		(*disj 4)
		;(*disj 13)
		done))

(define <ProperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <Sexpr>) ) *star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) s ))
		done))

(define <ImproperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <Sexpr>) ) *plus
		(*parser (word "."))
		(*delayed (lambda () <Sexpr>) )
		(*parser (word ")"))
		(*caten 5)
;		(*pack-with (lambda (prefix sexpr1 p sexpr2 suffix) `(,@sexpr1) ) )
		done))

(define <Vector>
	(new
		(*parser (word "#"))
		(*parser (word "("))
		(*delayed (lambda () <Sexpr>) ) *star
		(*parser (word ")"))
		(*caten 4)
		(*pack-with (lambda (pre s suf) s ))
		done))

(define <Quoted>
	(new
		(*parser (word "'"))
		(*delayed (lambda () <Sexpr>) )
		(*caten 2)
		done))

(define <QuasiQuoted>
	(new
		(*parser (word ""))
		(*delayed (lambda () <Sexpr>))
		(*caten 2)
		done))

(define <Unquoted>
	(new
		(*parser (char #\,))
		(*delayed (lambda () <Sexpr>))
		(*caten 2)
		done))

(define <Natural>
  (new (*parser <digit-0-9>) *star
       (*pack (lambda (s)  (string->number (list->string `(,@s)))))
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <nat>)

       (*disj 3)

       done))
       
(define <Number>
    (new (*parser <Integer>)
         (*parser <Natural>)
         
         (*disj 2)
         
         done))

;;fractions
(define <rat>
  (new (*parser <int>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))