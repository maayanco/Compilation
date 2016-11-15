(include "pc.scm")

;;(define <Boolean> 
;;	(new (*parser (char #\#))
;;		(*parser (char #\t))
;;		(*caten 2)
;;		(*pack-with  (lambda (a s) #t))
;;		(*parser (char #\#))
;;		(*parser (char #\f))
;;		(*caten 2)
;;		(*pack-with  (lambda (a s) #f))
;;		(*disj 2)
;;		done))

(define maayan
	(lambda () (char? #\x41)))

(define avi
	(lambda ()   (integer->char (string->number (list->string `(#\3 #\b #\b) ) ) )))

(define h
	(lambda () (integer->char 65)))

(define <Boolean> 
	(new
		(*parser (word-ci "#t"))
		(*pack (lambda (_) #t))

		(*parser (word-ci "#f"))
		(*pack (lambda (_) #f))

		(*disj 2)
		done))

;; Chars

;(define <CharPrefix2>
;	(new 
;		(*parser (word "#\\"))
;		(*pack (lambda (lst) (car lst)))
;		done))

(define <CharPrefix> 
	(new	(*parser (char #\#))
			(*parser (char #\\)) 
		    (*caten 2)
		  	;(*pack-with (lambda (a b)  (list (integer->char 35)(integer->char 92))))   
		    ;(*pack-with  (lambda (a b) (quote #\\)) )
		done))


(define <digit-greater-space> (range #\space #\~) )

(define <VisibleSimpleChar> 
	(new (*parser <digit-greater-space>) *star
	done))

(define <NamedChar>
	(new
		(*parser (word "lambda"))
		(*pack (lambda (_) (integer->char 955)))
		(*parser (word "newline"))
		(*pack (lambda (_) (integer->char 10)))
		(*parser (word "nul"))
		(*pack (lambda (_) (integer->char 0)))
		(*parser (word "page"))
		(*pack (lambda (_) `page ))
		(*parser (word "return"))
		(*pack (lambda (_) `return ))
		(*parser (word "space"))
		(*pack (lambda (_) `space ))
		(*parser (word "tab"))
		(*pack (lambda (_) `tab ))
		(*disj 7)
		done))

(define <HexChar>
	(new (*parser (range #\0 #\9) )
		(*parser (range #\a #\f) )
		;(pack (lambda (a) (char->integer a)))
		(*disj 2)
		;(*pack (lambda (a) (list->string `(,a)) ))
	done))

;(lambda (a s) (string->number (list->string	`(,a ,@s)))))

(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*pack (lambda (_) `#\x))
		(*parser <HexChar>) *plus
		;(*pack (lambda (s) (string->symbol (list->string`(,@s)) )))
		(*caten 2)
		;(*pack-with (lambda (a s) (integer->char (+ 24 (string->number (list->string `(,@s) )) )) ))
		(*pack-with (lambda (a s) (integer->char (string->number (list->string `(,@s) )  16) )))
		;(*pack-with (lambda (a s) (list->string `(,a ,s)) )))
		;(*pack-with (lambda (a s) (string->symbol (list->string `(,@s))) ))
		done))


(define mm (string->number "#\\b" ))

(define <Char>
	(new 
		(*parser <CharPrefix>)

		(*parser <NamedChar>)
		(*parser <HexUnicodeChar>)

		(*parser <VisibleSimpleChar>)
		(*pack (lambda (a) (string->symbol (list->string `(,a) ))))

		(*disj 3)

		(*caten 2)
		(*pack-with (lambda (a s) `( ,@s) ))
	;;	(*pack-with (lambda (a s) (list->string `(,@a ,@s)) ))
		done))

