# Compilation

Summary:

19/11 - 
- Wrote a file including tests to all terminals up to Vector.
- Tested and corrected (including the packing of everything up to Vector.
- Regarding Symbol, if i attempt to make sure the returned value is a symbol then the following scenario occurs:
(test-string <Symbol> "1")
==>  \x31
Should we return a string then?
- Problem with vector:
(test-string <Vector> "#(abc/2#\\Lambda)")
((match #(((#\a #\b #\c #\/ #\2) #\λ))) (remaining ""))

Left To Do:
- handle comments and whitespaces, there is the code mayer wrote and uploaded but it is still unclear in what staged are we supposed to use these methods to identify these comments/whitespaces.
Perhaps in the infix.

- Infix - the entire grammer.
