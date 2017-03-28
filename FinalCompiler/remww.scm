(define remww
  (lambda (lst-of-insts)
    (let ((new-lst (remww-helper lst-of-insts)))
      (if (equal? new-lst lst-of-insts)
          new-lst
          (remww new-lst)))
    ))

(define remww-helper
  (lambda (lst)
    (cond ((equal? lst '())  lst)
          ((is-record-necessary (car lst) (cdr lst)) (cons (car lst) (remww-helper (cdr lst))))
          (else (remww (cdr lst))))
    ))


;;record can't be null!
(define is-record-necessary
  (lambda (record rest-lst)
    (let ((record-reads-lst (cadr record))
          (record-write-lst (caddr record)))
          (ormap (lambda (register) (is-record-necessary-helper register rest-lst)) record-write-lst))
    ))

(define is-record-necessary-helper
  (lambda (register rest-lst)
    (cond ((equal? rest-lst '()) '())
          (else (let* ((new-record (car rest-lst))
                       (new-record-read (cadr new-record))
                       (new-record-write (caddr new-record)))
                  (cond ((member register new-record-read) #t)
                        ((member register new-record-write) #f)
                        (else (is-record-necessary-helper register (cdr rest-lst))))
                        )))
    ))

