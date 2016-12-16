
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
   
(define is-smallest?
  (lambda (expr)
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
     (lambda (item) (cond ((and (list? item) (not-empty? item) (is-smallest? item))
                               (if (equal? item item-to-replace) gensym-name item))
                              
                              ((and (list? item) (not-empty? item)) 
                               ( go-over-and-replace item item-to-replace gensym-name))
                              
                              ((or (not (list? item)) (empty? item)) 
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


  
(define go-over-exp-better
  (lambda (exp original-exp)
          (if (or (not (list? exp)) (not (not-empty? exp)))
              (if (equal? exp `())
                  original-exp 
                    exp)
                  (cond ((and (is-smallest? exp) (not (quote? exp)) (> (get-occurrences-num original-exp exp) 1))  
                         (let* ((gensym-name (gensym))
                                      (replaced-exp (go-over-and-replace original-exp exp gensym-name)))
                                 (update-saved-vars (list gensym-name exp))
                           replaced-exp))
                        ((not (quote? exp))
                         (let* ((ret-car-expr (go-over-exp-better (car exp) original-exp))) 
                               (if (or (equal? ret-car-expr (car exp)) (equal? ret-car-expr original-exp))
                                   (go-over-exp-better (cdr exp) original-exp)
                                   ret-car-expr)))
                        (else exp)))))
                             
                    

(define apply-goe-rec
  (lambda (exp original-exp)
    (let ((new-exp (go-over-exp-better exp original-exp)))
      (if (equal? new-exp exp)
          new-exp
          (apply-goe-rec new-exp new-exp)))))

(define delete-once
  (lambda (item lst)
    (if (and (list? lst) (not (equal? lst '())))
        (cond
          ((equal? item (car lst)) (cdr lst))
          (else (cons (car lst) (delete-once item (cdr lst)))))
     (begin
       item))))

(define replace
 (lambda (exp old-item new-item)
   (if (equal? exp old-item)
       new-item
       (if (and (list? exp) (not (equal? exp `())))
           (cons (replace (car exp) old-item new-item) (replace (cdr exp) old-item new-item))
           exp)
    )))


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

(define get-pair-with-dup-2
  (lambda (params-lst body)
    (let ((new-lst (map
                   (lambda (item)
                     (if (equal? (get-occurrences-num (cons params-lst body) (car item)) 2) item '() ))
                   params-lst)))
      (filter (lambda (item) (not (equal? `()  item))) new-lst))))


(define remove-dups
  (lambda (body lst)

    (let ((list-of-pairs (get-pair-with-dup-2 lst body)))
      (if (or (equal? list-of-pairs `()) (equal? list-of-pairs `(())))
          lst
            (let ((lst-after-delete (delete-once (car list-of-pairs) lst))) 
                     (let ((lst-after-replace (replace lst-after-delete (caar list-of-pairs) (cadar list-of-pairs)))
                           (body-after-replace (replace body (caar list-of-pairs) (cadar list-of-pairs))))
                       (remove-dups body lst-after-replace)))))))   


(define cse-2
  (lambda (exp)
    (set! saved-vars '())
    (let ((res (apply-goe-rec exp exp)))
      (set! saved-vars (remove-dups res saved-vars))
      (cond ((equal? (length saved-vars) 0) res)
             ((> (length saved-vars) 1) `(let* ,saved-vars ,res))
             (else `(let ,saved-vars ,res))))))


