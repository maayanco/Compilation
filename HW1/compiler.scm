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


; Should be case sensitive
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


;Should be case sensitive
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



(define <StringHexChar>
	(new 
		(*parser (word "\\"))
		(*parser (word "x"))
		(*parser <HexChar>) *star
		(*parser (word ";"))
		(*caten 4)
		(*pack-with (lambda (pre x lst comma) (list->string lst)  ))
	;	(*parser (char #\\))
	;	(*parser (char #\x))
	;	(*parser <HexChar> ) *star
	;	(*parser (char #\;))
	;	(*caten 4)
		done))


(define <StringChar>
	(new 
		(*parser <StringMetaChar>)
		(*parser <StringHexChar>)
		(*parser <StringLiteralChar>)
		(*disj 3)
		done))



(define <StringCharNoApostrophes>
	(new 
		(*parser <StringChar>) *star
		(*parser (char #\"))
		*diff
		(*parser (char #\\))
		*diff
		done))

(define <String>
	(new
		(*parser (word "\""))
		(*parser <StringChar>) 
		(*guard (lambda (a) (not (equal? a #\"))  ))
		*star
		(*parser (word "\""))
		(*caten 3)
		(*pack-with (lambda (pre lst post) (list->string `(,@lst))  ))
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


(define <Natural>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with (lambda (a s) (string->number (list->string `(,a ,@s)))))

       (*disj 2)
       done))

;(define <Natural>
;  (new (*parser <digit-0-9>) *star
;       (*pack (lambda (s)  (string->number (list->string `(,@s)))))
;       done))

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

;(define <Integer>
;  (new (*parser (char #\+))
;       (*parser <Natural>)
;       (*caten 2)
;       (*pack-with (lambda (++ n) n))

;       (*parser (char #\-))
;       (*parser <Natural>)
 ;      (*caten 2)
;       (*pack-with (lambda (-- n) (- n)))

;       (*parser <Natural>)

;       (*disj 3)

;       done))
       
 (define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with (lambda (num div den) (/ num den)))
       done))


(define <Number>
    (new 
         (*parser <Fraction>)
         (*parser <Integer>)
         (*disj 2)
         
         done))

;;fractions




(define <ProperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr>) ) *star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) s ))
		done))


(define <ImproperList>
	(new
		(*parser (word "("))
		(*delayed (lambda () <sexpr>) ) *plus
		(*parser (word "."))
		(*delayed (lambda () <sexpr>) )
		(*parser (word ")"))
		(*caten 5)
;		(*pack-with (lambda (prefix sexpr1 p sexpr2 suffix) `(,@sexpr1) ) )
		done))

(define <Vector>
	(new
		(*parser (word "#("))
		(*delayed (lambda () <sexpr>) ) ;*star
		(*parser (word ")"))
		(*caten 3)
		(*pack-with (lambda (pre s suf) (vector s)  ))
		done))

(define <Quoted>
	(new
		(*parser (word "'"))
		(*delayed (lambda () <sexpr>) )
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quote s)))
		done))

(define <QuasiQuoted>
	(new
		(*parser (word "`"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'quasiquote s)))
		done))

(define <Unquoted>
	(new
		(*parser (word ","))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote s)))
		done))

(define <UnquoteAndSpliced>
	(new
		(*parser (word ",@"))
		(*delayed (lambda () <sexpr>))
		(*caten 2)
		(*pack-with (lambda (a s) (list 'unquote-splicing s) ))
		done))


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

(define <sexpr>
	(new
		(*parser <ImproperList>)
		(*parser <ProperList>)
		(*parser <Vector>) ;should be packed
		(*parser <Boolean>)
		(*parser <Quoted>)
		(*parser <QuasiQuoted>)
		(*parser <Unquoted>)
		(*parser <UnquoteAndSpliced>)
		(*parser <Number>)
		(*parser <Char>)
		(*parser <Symbol>)
		(*parser <String>)

		;(*parer <InfixExtension>)
		(*disj 12)
		;(*disj 13)
		done))