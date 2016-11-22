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
		;(*pack (lambda (a) (string a)  ))
		*diff
		done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))

(define <StringMetaChar2>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
    
       (*disj 6)
       done))


(define <StringMetaChar> 
	(new

		(*parser (word-ci "\\\""))
		(*pack (lambda (_) "\""))
		;(*pack (lambda (_) #\"))

		(*parser (word-ci "\\t"))
		(*pack (lambda (_) "\t"))
		;(*pack (lambda (_) #\t))

		(*parser (word-ci "\\f"))
		(*pack (lambda (_) "\f"))
		;(*pack (lambda (_) #\f))

		(*parser (word-ci "\\n"))
		(*pack (lambda (_) "\n"))
		;(*pack (lambda (_) #\n))

		(*parser (word-ci "\\r"))
		(*pack (lambda (_) "\r"))
		;(*pack (lambda (_) #\r))

		(*parser (word-ci "\\\\"))
		(*pack (lambda (_) "\\"))
		;(*pack (lambda (_) #\\))
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
		;(*guard (lambda (a) (not (eq? a "\""))))
		;(*pack (lambda (a) (display "StringMetaChar") a))
		(*parser <StringHexChar>)
		;(*guard (lambda (a) (not (eq? a "\""))))
		;(*pack (lambda (a) (display "StringHexChar") a))
		(*parser <StringLiteralChar>)
		;(*guard (lambda (a) (not (eq? a "\""))))
		;(*pack (lambda (a) (display "StringLiteralChar") a))
		(*disj 3)
		done))



(define <String>
	(^<skipped*>
	(new
		(*parser (word "\""))
		;(*pack (lambda (a)  (display "got brk1") (display "||") a ))
		(*parser <StringChar>) 
		(*guard (lambda (a) 
			;(display "i got ") (display a) (display " and returned ")
			;(display (char? a)) 
			;(display (and (not (eq? a #\\)) (not (eq? a #\")) ))
			(and (not (equal? a #\\)) (not (equal? a #\")) )))
		*star
		;(*pack (lambda (a)  (display "got lst") a))
		(*parser (word "\""))
		;(*pack (lambda (a)  (display "got brk2") a ))
		(*caten 3)
		(*pack-with (lambda (pre lst post) (list->string `(,@lst))  ))
		;(*pack-with (lambda (pre lst post) (list->string lst)))
		done)))
