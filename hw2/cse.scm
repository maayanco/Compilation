
;; Taken from my ppl Assignment 3 question 1ab that i wrote!!
(print-gensym #f)

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))

(define empty?
  (lambda (a) (if (equal? a `()) #t #f)))
               
(define powerset
  (lambda (s)
    (if (empty? s)
        `(())
        (fold-right append '()
               (map (lambda (item) (let ((newElement (car s))) (list item (cons newElement item)))) (powerset (cdr s)))))))

(define is-smallest?
  (lambda (expr)
    ;(display "expr is:") (display expr) (display "\n")
    (if (not (list? expr))
        #f
         (begin 
                (andmap (lambda (item) (if (or (quote? item) (not (list? item))) #t #f)) expr))
         )))

(define not-empty?
  (lambda (a) (if (equal? a '()) #f #t)))

(define saved-vars '())

(define go-over-and-replace
  (lambda (exp item-to-replace gensym-name)
    (map
     (lambda (item) (cond ((and (list? item) (not-empty? item) (is-smallest? item)) ;;we found the smallest element - we will just disp
                               (if (equal? item item-to-replace) gensym-name item))
                              
                              ((and (list? item) (not-empty? item)) ;;item is a subexpression! we need to scan it as well...
                               ( go-over-and-replace item item-to-replace gensym-name))
                              
                              ((or (not (list? item)) (empty? item)) ;;item is not a list or is an empty list return it as it is..
                               (if (equal? item item-to-replace)
                                   gensym-name
                                   item))
                              ))
     exp)))


(define get-occurrences-num
  (lambda (exp item-to-find)
    (cond
      ((or (not (list? exp)) (equal? exp `())) 0)
      ((list? exp)
       (if (equal? (car exp) item-to-find)
           (+ 1 (get-occurrences-num (car exp) item-to-find) (get-occurrences-num (cdr exp) item-to-find))
           (+ 0 (get-occurrences-num (car exp) item-to-find) (get-occurrences-num (cdr exp) item-to-find))))
           )))
    
(define update-saved-vars
  (lambda (lst)
    (set! saved-vars (cons lst saved-vars))))



;(define go-over-exp-much-better
  
;;;;; there is a problemmm here!!! 
(define go-over-exp-better
  (lambda (exp original-exp)
          (if (or (not (list? exp)) (not (not-empty? exp)))
              (if (equal? exp `())
                  (begin ;(display "\n OUT!") (display exp) (display "\n")
                    original-exp)
                  (begin ;(display "\n im outii") (display exp) (display "\n")
                         exp))

              (begin
              ;      (display "\n i'm in!!")
                  ;;all the rest of this lambda happens only if exp is a list and not empty..
                ;(display "hellllllllo")
               ; (display "\n\n is this a valid smallest? ")          (display (is-smallest? exp))
                                                             ;        (display (not (quote? exp)))
                                                          ;           (display (> (get-occurrences-num original-exp exp) 1))
                                                            ;         (display "\n")
              ;  (display " \n and the expression is:") (display exp) (display "\n")
                  (cond ((and (is-smallest? exp) (not (quote? exp)) (> (get-occurrences-num original-exp exp) 1)) ;; we have found a good expression
                      ;;then clause
                      (begin   
                               (let* ((gensym-name (gensym))
                                      (replaced-exp (go-over-and-replace original-exp exp gensym-name)))
                                 (update-saved-vars (list gensym-name exp))
                                 replaced-exp)))
                        ;; we didn't find a good expression, we need to search for it in the car or cdr
                        ((not (quote? exp))
                             (let* ((ret-car-expr (go-over-exp-better (car exp) original-exp))) ;; we went inside the car!!
                              ; (display "\n i'm mmmaayan \n") 
                               (if (or (equal? ret-car-expr (car exp)) (equal? ret-car-expr original-exp))
                                   ;;then : nothing changed, we need to search in the cdr
                                   (begin ;(display "entering the cdr..")
                                          (go-over-exp-better (cdr exp) original-exp))
                                   ;;else - it did change! we will return the changed exp
                                   (begin ;(display "car has changed and we'll just return it\n")
                                          ;(display "car before:") (display (car exp)) (display "\n")
                                         ; (display "car after:") (display ret-car-expr) (display "\n")
                                          ret-car-expr))))
                        (else exp))))))
                             
                    

(define apply-goe-rec
  (lambda (exp original-exp)
    ;(display "\n\n") (display "current exp:") (display exp) (display "\n")
    (let ((new-exp (go-over-exp-better exp original-exp)))
          (if (equal? new-exp exp)
              ;;if nothing changed in this iteration than that's it! we return new-exp
              (begin ;(display "hello\n")
                     new-exp)
              ;;else clause- if the expression did change, we want to
              (begin ;(display "changed! go again: ") (display " before: ") (display exp) (display " after: ") (display new-exp) (display " \n")
                     (apply-goe-rec new-exp new-exp))))))

(define delete-once
  (lambda (item lst)
    ;(display "inside delete-once \n")
    ;(display "item:") (display item) (display "\n") (display "lst") (display lst) (display "\n") 
    (if (and (list? lst) (not (equal? lst '())))
        (cond
          ((equal? item (car lst)) (cdr lst))
          (else (cons (car lst) (delete-once item (cdr lst)))))
     (begin
       ;(display "oops")
       item))))

(define replace
 (lambda (exp old-item new-item)
 ;  (display "replace \n")
   ;(display "current exp:") (display exp) (display "\n")
   (if (equal? exp old-item)
       new-item
       (if (and (list? exp) (not (equal? exp `())))
           (cons (replace (car exp) old-item new-item) (replace (cdr exp) old-item new-item))
           exp)
    )))


;; at the beggining the lst-of-pairs and original-lst are the same (they are both saved-vars)
;; but when we go over a pair
(define remove-dups
  (lambda (body lst-of-pairs updated-lst)
    (if (equal? lst-of-pairs `())
        updated-lst
        (let ((var (caar lst-of-pairs))
              (val (cdar lst-of-pairs)))
          (if (equal? (get-occurrences-num (cons body updated-lst) var) 2)
              (remove-dups body (cdr lst-of-pairs) (replace (delete-once pair lst) var val))
              (remove-dups body (cdr lst-of-pairs) updated-lst)
              )))))
     ;; i want to go over the list, if (car lst) is occ 2 as i want, i take it and change lst and envoke remove-dups on the new lst
     ;; if the car doesn't fit my demands i continue to cdr lst 

(define get-pair-with-dup-2
  (lambda (params-lst body)
   ; (display "inside get-pair-with-dup-2\n")
  ;  (display "params-lst: ")(display params-lst) (display "\n")
  ;  (display "body: ")(display body) (display "\n")
    (let ((new-lst (map
                   (lambda (item)
                     (if (equal? (get-occurrences-num (cons params-lst body) (car item)) 2) item '() ))
                   params-lst)))
      ;(display new-lst)
      ;(display "outside new-lst\n")
      ;(display "new-lst:") (display new-lst)
     ; (display "new list:") (display (filter (lambda (item) (not (equal? `()  item))) new-lst)) (display "\n")
      (filter (lambda (item) (not (equal? `()  item))) new-lst))))
      ;(display (fold-right append '() new-lst)) (display "\n")
       ;(fold-right append '() new-lst))))

(define remove-dups
  (lambda (body lst)
    ;(display "ver\n")
    (let ((list-of-pairs (get-pair-with-dup-2 lst body)))
      (if (or (equal? list-of-pairs `()) (equal? list-of-pairs `(())))
          lst
          (begin
            (let ((lst-after-delete (delete-once (car list-of-pairs) lst)))
              (begin ;(display "lst after delete: ") (display lst-after-delete) (display "\n")
                     (let ((lst-after-replace (replace lst-after-delete (caar list-of-pairs) (cadar list-of-pairs)))
                           (body-after-replace (replace body (caar list-of-pairs) (cadar list-of-pairs))))
                      ; (display "lst after replace:") (display lst-after-replace) (display "\n")
                       ;(display "body: ") (display body-after-replace) (display "\n")
                       (remove-dups body lst-after-replace)))))))))


        


(define cse-2
  (lambda (exp)
    ;(display "\n current exp im working on:") (display exp) (display "\n")
    (set! saved-vars '())
    (let ((res (apply-goe-rec exp exp)))
    ;  (display "res before:") (display saved-vars) (display "\n")
     ; (display "body before:") (display res) (display "\n")
      (set! saved-vars (remove-dups res saved-vars))
      (cond ((equal? (length saved-vars) 0) res)
             ((> (length saved-vars) 1) `(let* ,saved-vars ,res))
             (else `(let ,saved-vars ,res))))))


