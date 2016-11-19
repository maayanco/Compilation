# Compilation

Summary:

20/11-
  - Added the comments tokens from Mayer's uploaded file.
  - Wrote the definitions for almost all the infix terminals.
  - Changed \<Symbol> to return a string.
  - Need to implement logic between the different infix actions, meaning precedence.
  ref:
  https://www.facebook.com/groups/comp171/permalink/919407011494147/?comment_id=920794048022110&comment_tracking=%7B%22tn%22%3A%22R7%22%7D
  - Need to incorporate the comments into infix (i think..), where do we take care of that?
  
19/11 - 
- Wrote a file including tests to all terminals.
- Tested and corrected (including packing).

Problems:
- In Symbol, if i attempt to make sure the returned value is a symbol then the following scenario occurs:
(test-string \<Symbol> "1")
==>  \x31
Should we return a string then?
- Problem when testing vector:
(test-string <sexpr> "#(#\\Lambda1abc)")
((match #(#\λ 1 (#\a #\b #\c))) (remaining ""))
there is some type symbol, char or string that causes the returned value of abc to be (#\a #\b #\c), something isn't packed properly..


Left To Do:
- Test sexpr as a whole
- handle comments and whitespaces, there is the code mayer wrote and uploaded but it is still unclear in what staged are we supposed to use these methods to identify these comments/whitespaces.
Perhaps in the infix.
- Infix - the entire grammer.
