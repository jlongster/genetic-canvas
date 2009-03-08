
(define-macro (define-test name . code)
  `(begin
     (display (string-append "Testing "
                             (symbol->string ',name)
                             "...  "))
     ,@code
     (display "OK\n")))

(define-type assert-exception
  id: 214FB272-AC60-4F1D-87CA-8BD9EC8F164B
  expr)

(define-type assert-equal-exception
  id: 1A637BD4-DB81-4AF4-BEF7-BBF19AC6A9C9
  expr1
  expr2)

(define-macro (assert expr)
  `(if (not ,expr)
       (raise (make-assert-exception ',expr))))

(define-macro (assert-equal expr1 expr2)
  `(if (not (equal? ,expr1 ,expr2))
       (raise (make-assert-equal-exception ',expr1 ',expr2))))
