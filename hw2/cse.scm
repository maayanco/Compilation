
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
                ;(display "hellllllllo")
               (display "is this a quote expression? ") (display (quote? exp)) (display " and the expression is:") (display exp) (display "\n")
                  (if (and (is-smallest? exp) (not (quote? exp)) (> (get-occurrences-num original-exp exp) 1)) ;; we have found a good expression
                      ;;then clause
                      (begin   
                               (let* ((gensym-name (gensym))
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


(define go-over-exp-better
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
                ;(display "hellllllllo")
                ;(display "is this a quote expression? ") (display (quote? exp)) (display " and the expression is:") (display exp) (display "\n")
                  (cond ((and (is-smallest? exp) (not (quote? exp)) (> (get-occurrences-num original-exp exp) 1)) ;; we have found a good expression
                      ;;then clause
                      (begin   
                               (let* ((gensym-name (gensym))
                                      (replaced-exp (go-over-and-replace original-exp exp gensym-name)))
                                 (update-saved-vars (list gensym-name exp))
                                 replaced-exp)))
                        ((not (quote? exp))
                             (let* ((ret-car-expr (go-over-exp-better (car exp) original-exp))) ;; we went inside the car!!
                               (if (equal? ret-car-expr (car exp))
                                   ;;then : nothing changed, we need to search in the cdr
                                   (go-over-exp-better (cdr exp) original-exp)
                                   ;;else - it did change! we will return the changed exp
                                   ret-car-expr)))
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

 (define delete
  (lambda (item lst)
    (cond
     ((equal? item (car lst)) (cdr lst))
     (else (cons (car lst) (delete item (cdr lst)))))))


  
(define remove-duplicates2
  (lambda (body)
    ;; go over saved-vars and body
    (display "hello i'm inside remove-duplicates")
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

(define remove-duplicates
  (lambda (body)
    (begin
    ;; go over saved-vars and body
    ;(display "hello i'm inside remove-duplicates")
    ;(display "i got the body:") (display body)
   ; (display "and saved vars is:") (display saved-vars) (display "\n")
    (let ((exp (cons saved-vars body)))
       ;;for every element in saved-vars, go over exp and check:
       (map
        (lambda (element)
          (let ((var (car element)))
            (if (equal? (get-occurrences-num exp var) 2)
                (begin ;(display "\nvar is:") (display var) (display "\n")
                (set! saved-vars (delete var saved-vars))));;(instead of var should i send (car element?)
            ))
        saved-vars))
    )))

(define replace
 (lambda (exp old-item new-item)
   ;(display "current exp:") (display exp) (display "\n")
   (if (equal? exp old-item)
       new-item
       (if (and (list? exp) (not (equal? exp `())))
           (cons (replace (car exp) old-item new-item) (replace (cdr exp) old-item new-item))
           exp)
    )))

(define test
  (lambda (exp)
    (map (lambda (item) (display "item") (delete item exp)) exp)
    ))

(define remove-dups
  (lambda (body orig-saved-vars)
    (display "In remove dups\n")
    (let ((exp (cons body orig-saved-vars)))
      (map
       (lambda (pair)
         (if (equal? (get-occurrences-num exp (car pair)) 2)
             (begin (display "we have occ number 2!\n")
                    (let ((var (car pair)) (val (cadr pair)))
                      (set! orig-saved-var (delete pair orig-saved-vars))
                      (set! orig-saved-vars (replace orig-saved-vars var val))))
             pair))
             ;;first we will remove the pair we want
             ;;go over the entire body and saved-vars, every where where there is (car pair) i need to replace it with (cadr pair)
             ;;now we delete pair from saved-vars
       orig-saved-vars)
      (display "orig-saved-vars:") (display orig-saved-vars) (display "\n")
     )))

(define cse-2
  (lambda (exp)
    (set! saved-vars '())
    (let ((res (apply-goe-rec exp exp)))
      (remove-dups res saved-vars)
      (cond ((equal? (length saved-vars) 0) res)
             ((> (length saved-vars) 1) `(let* ,saved-vars ,res))
             (else `(let ,saved-vars ,res))))))


(define cse-22
  (lambda (exp)
    ;(display "current exp im working on:") (display exp) (display "\n")
    (set! saved-vars '())
    (let ((res (apply-goe-rec exp exp)))
      (remove-duplicates res)
      (cond ((equal? (length saved-vars) 0) res)
             ((> (length saved-vars) 1) `(let* ,saved-vars ,res))
             (else `(let ,saved-vars ,res))))))
    