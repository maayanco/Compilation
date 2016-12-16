
;; Taken from my ppl Assignment 3 question 1ab that i wrote!!

(define flatmap
  (lambda (proc seq)
    (foldr append (list) (map proc seq))))

(define permutations
  (lambda (s)
    (if (set-empty? s)
        (list empty)
        (flatmap
         (lambda (x)
           (map (lambda (p) (cons x p))
                (permutations (set-remove-el x s))))
         (set->list s)))
    ))

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
    (if (not (list? expr))
        #f
        (andmap (lambda (item) (if (not (list? item)) #t #f)) expr))))

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


(define go-over-exp
  (lambda (exp original-exp)
          (if (or (not (list? exp)) (not (not-empty? exp)))
              (if (equal? exp `())
                  (begin ;(display "\n i'm super `() out")
                    original-exp)
                  (begin ;(display "\n im out") (display exp)
                         exp))

              (begin
              ;      (display "\n i'm in!!")
                  ;;all the rest of this lambda happens only if exp is a list and not empty..
                  (if (and (is-smallest? exp) (> (get-occurrences-num original-exp exp) 1)) ;; we have found a good expression
                      ;;then clause
                      (begin   
                               (let* ((gensym-name (symbol->string (gensym)))
                                      (replaced-exp (go-over-and-replace original-exp exp gensym-name)))
                                 (update-saved-vars (list gensym-name exp))
                                 replaced-exp))
                      
                      ;;we haven't found a recurring smallest, we will search inside the car or in the cdr
                      (begin 
                      (let* ((ret-car-expr (go-over-exp (car exp) original-exp)))
                         (if (equal? ret-car-expr (car exp))
                             ;;then : nothing changed, we need to search in the cdr
                             (go-over-exp (cdr exp) original-exp)
                             ;;else - it did change! we will return the changed exp
                              ret-car-expr))))))))
                             

;(define go-over-exp
;  (lambda (exp)
;    (cond ((and (list? exp) (not-empty? exp) (is-smallest? (car exp))) (display "in1") ;;we have found a smallest item!!
;           (if (> (get-occurrences-num exp (car exp)) 1) ;;this smallest item appears more than once!!
;               ;;then clause
;               (begin   (let* ((gensym-name (gensym))
;                      (replaced-exp (go-over-and-replace exp (car exp) gensym-name)))
;                 (update-saved-vars (list gensym-name (car exp)))
;                 replaced-exp))
;               ;;else clause:
;               (begin  (car exp))))
;          ((and (list? exp) (not-empty? exp) (list? (car exp)) (not (is-smallest? (car exp)))) (display "in2") (go-over-exp (car exp))) ;; search for smallest item inside the first item of this exp
          
;          ((and (list? exp) (not-empty? exp) (not-empty? (cdr exp))) (display "in3") (cons (car exp) (go-over-exp (cdr exp)))) ;; we want to progress to the cdr of this exp and maybe i'll find the smallest item there..
;          )))

(define counter 0)

(define apply-goe-rec3242342
  (lambda (exp original-exp)
    (set! counter (+ counter 1))
    ;(display "\n\n") (display "current exp:") (display exp) (display "\n")
    (if (equal? counter 8)
        exp
    (let ((new-exp (go-over-exp exp original-exp)))
          (if (equal? new-exp exp)
              ;;if nothing changed in this iteration than that's it! we return new-exp
              (begin ;(display "hello\n")
                     new-exp)
              ;;else clause- if the expression did change, we want to
              (begin ;(display "changed! go again: ") (display " before: ") (display exp) (display " after: ") (display new-exp) (display " \n")
                     (apply-goe-rec new-exp new-exp)))))))

(define apply-goe-rec
  (lambda (exp original-exp)
    (set! counter (+ counter 1))
    ;(display "\n\n") (display "current exp:") (display exp) (display "\n")

    (let ((new-exp (go-over-exp exp original-exp)))
          (if (equal? new-exp exp)
              ;;if nothing changed in this iteration than that's it! we return new-exp
              (begin ;(display "hello\n")
                     new-exp)
              ;;else clause- if the expression did change, we want to
              (begin ;(display "changed! go again: ") (display " before: ") (display exp) (display " after: ") (display new-exp) (display " \n")
                     (apply-goe-rec new-exp new-exp))))))

 (define delete
  (lambda (item saved-var)
    (cond
     ((equal? item (car saved-vars)) (cdr saved-vars))
     (else (cons (car saved-vars) (delete item (cdr saved-vars)))))))

;(define remove-element-from-saved-vars
;  (lambda (elm-to-remove)
;    (map (lambda (item) (if (equal? item item-to-find)) delete saved-vars)
;    ))


  
(define remove-duplicates
  (lambda (body)
    ;; go over saved-vars and body
    (let ((exp (cons saved-vars body)))
       ;;for every element in saved-vars, go over exp and check:
       (map
        (lambda (element)
          (let ((var (car element)))
            (if (equal? (get-occurrences-num exp var) 2)
                (begin ;(display "\nvar is:") (display var) (display "\n")
                (set! saved-vars (delete var saved-vars))));;(instead of var should i send (car element?)
            ))
        saved-vars)
    )))

(define cse-2
  (lambda (exp)
    (set! saved-vars '())
    (let ((res (apply-goe-rec exp exp)))
      (remove-duplicates res)
      (if (> (length saved-vars) 1)
          `(let* ,saved-vars ,res)
          `(let ,saved-vars ,res)))))
    
(define cse3
  (lambda (expr) 
    (map
     (lambda (item) (cond ((and (list? item) (not-empty? item) (is-smallest? item))
                           ; now we should go over the rest!! of the elements and search if there is an element equal to item
                           ;; if we end the search and expr isn't changed - than we need to finish and conclude that our expression is the same
                           1)
                           ((and (list? item) (not-empty? item) (not (is-smallest? item))) ; now we should go deeper inside until we find the smallest!
                            2)
                           (else (display "item:") (display item) (display "\n") item)
                           ))
     expr)
    ))
    