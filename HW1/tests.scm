;;Tests

;; Comment: i think that <Char> should return char,
;<Boolean> should return boolean, and so on..

;;<Boolean>

(test-string <Boolean> "#f")
==> #t

(test-string <Boolean> "#f")
==> #f

(test-string <Boolean> "#T")
==> #t

(test-string <Boolean> "#F")
==>#f

Boolean shouldn't accept anything else!

;; <CharPrefix>
(test-string <CharPrefix> "#\\")
==> #\\
is this return value okay?
is the input value ok?

;; <VisibleSimpleChar>
(test-string <VisibleSimpleChar> "a")
==> #\a

(test-string <VisibleSimpleChar> "!")
==> #\!

(test-string <VisibleSimpleChar> " ")
==> fail!

(note: returns the char)

;; <NamedChar>
(test-string <NamedLambda> "lambda")
==> #\λ

(test-string <NamedLambda> "newline")
==>#\newline

(test-string <NamedLambda> "nul")
==>#\nul

(test-string <NamedLambda> "page")
==>#\page

(test-string <NamedLambda> "return")
==>#\return

(test-string <NamedLambda> "space")
==>#\space

(test-string <NamedLambda> "tab")
==>#\tab

;; <HexChar>
(test-string <HexChar> "0")
==> 0

(test-string <HexChar> "a")
==>a

(Question: one one hand the assignment specifies that everything should be case insensitive (not including VisibleSimpleChar and StringLiteralChar)
	but on the other hand, here it specifies a to z 
	should it include A to Z (as in capital)?
(Another question: should the input entered be a string or a char?) we take a string..

;; <HexUnicodeChar>
(test-string <HexUnicodeChar> "x0")
==>#\nul 

(test-string <HexUnicodeChar> "x")
==> fail!

(test-string <HexUnicodeChar> "xa13")
==>#\ਓ

(test-string <HexUnicodeChar> "X0") ;; test capital X
==> #\nul


;; <Char>
;; should return char i think..
(test-string <Char> "#\\a") ;;tests VisibleSimpleChar inside char
==>#\a

(test-string <Char> "#\\ ") ;;tests VisibleSimpleChar inside char
==> fail!

(test-string <Char> "#\\~") ;;tests VisibleSimpleChar inside char
==> #\~

(test-string <Char> "#\\Lambda") ;;tests NamedChar inside char
==> #\λ

(test-string <Char> "#\\xa13") ;; tests HexUnicodeChar inside char
==>#\ਓ

;; <Natural>
(test-string <Natural> "0123")
==> 123

(test-string <Natural> "0000")
==> 0

(test-string <Natural> "")
==> fail!

;; <Integer>
(test-string <Integer> "+000123")
==> 123

(test-string <Integer> "-00123")
==> -123

(test-string <Integer> "000123")
==> 123

;; <Fraction>
(test-string <Fraction> "0/123")
==> 0

(test-string <Fraction> "-4/123")
==> -4/123

(test-string <Fraction> "-4/0")
==>fail!

; Question: in Mayer's parser we receive:
; (test-string <sexpr> "-4/0")
; ((match \x2D;4/0) (remaining ""))
; why? is this the required result?

(test-string <Number> "-4/123")
==> -4/123

(test-string <Number> "+123")
==> 123


;; <StringLiteralChar>
;; Question: returns char, not string, ok?
(test-string <StringLiteralChar> "a")
==> #\a

(test-string <StringLiteralChar> "\\")
==> fail!

;; <StringMetaChar>
;; Question: returns char, not string, ok?
(test-string <StringMetaChar> "\\")
==> #\\

(test-string <StringMetaChar> "\"")
==> #\"

(test-string <StringMetaChar> "\t")
==> #\t

(test-string <StringMetaChar> "\f")
==> #\f

(test-string <StringMetaChar> "\n")
==>#\n

(test-string <StringMetaChar> "\r")
==>#\r

;; <StringHexChar> 
(test-string <StringHexChar> "\\x123;")
==> #\ģ

(test-string <StringHexChar> "\\X123;")
==> #\ģ

(test-string <StringHexChar> "\\x;")
==> ""
;; in mayer's compiler this is a fail
;; should it be here too?

;; <StringChar>
(test-string <StringChar> "a") ;; test StringLiteral inside Char
==> #\a

(test-string <StringChar> "\\") ;;test StringMetaChar inside Char
==> #\\

(test-string <StringChar> "\r") ;; test StringMetaChar inside Char
==> #\r

(test-string <StringChar> "\x123;")
==> #\ģ



;; <String>
;; Must return string!
(test-string <String> "\"\\\"") ;;test StringMetaChar inside Char
==> "\\"
;; why does this fail in mayer's parser??

(test-string <String> "\"\r\"") ;;test StringMetaChar inside Char
==> "\r"

(test-string <String> "\"a\"") ;;test StringMetaChar inside Char
==> "a"

(test-string <String> "\"\x123;\"") ;;test StringMetaChar inside Char
==> "ģ"

;; <SymbolChar>
;; return value is a char, is this ok?
(test-string <SymbolChar> "+")
==> #\+

(test-string <SymbolChar> "0")
==> #\0

(test-string <SymbolChar> "A")
==> #\A

;; <Symbol>
;; return value is a char, is this ok?
(test-string <SymbolChar> "+")
==> #\+

(test-string <SymbolChar> "0")
==> #\0

(test-string <SymbolChar> "A")
==> #\A

;; PROBLEM! when trying to pack the result as a symbol, i get:
(test-string <Symbol> "1")
==>  \x31
;; so i turned it back so it will return chars..
;;OTHER PROBLEM : the returned value is a list i think,
;; it appears like this: (match: (#\1))
;; there are extra brackets wrapping the 1

;; <ProperList>
;; Question: How should i display the result?
;; as a list of sexpr's??
(test-string <ProperList> "(123#\\Lambda)")
==> (123 #\λ)

(test-string <ProperList> "()")
==> ()

;; <ImproperList>

(test-string <ImproperList> "(#\\Lambda.#\\Lambda)")
==> (#\λ . #\λ))

;;Vector
(test-string <Vector> "#(#\\Lambda#\\Lambda)")