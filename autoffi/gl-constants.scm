
(define (parser-error . args)
  (for-each
   (lambda (x)
     (if (not (string? x))
         (write x)
         (display x)))
   args)
  (newline)
  #f)

(define (num-token? token)
  (and (pair? token)
       (eq? (car token) 'num)
       (eq? (length token) 2)))

(define (make-num token)
  (cadr token))

(define (id-token? token)
  (and (pair? token)
       (eq? (car token) 'id)
       (eq? (length token) 2)))

(define (make-id token)
  (string->symbol (cadr token)))

(define (constant-token? token)
  (and (pair? token)
       (eq? (car token) 'pp-define)
       (eq? (length token) 3)))

(define (make-constant-expr token)
  (let ((id-token (cadr token))
        (val-token (caddr token)))
    (if (not (id-token? id-token))
        (parser-error "invalid id: " id-token)
        (let ((id (make-id id-token))
              (val (cond
                    ((id-token? val-token) (make-id val-token))
                    ((num-token? val-token) (make-num val-token))
                    (else (parser-error
                           "invalid constant value: "
                           val-token)))))
          (and val
               `(define ,id ,(if (symbol? val)
                                 `(lambda () ,val)
                                 val)))))))

(define (parse input-port output-port)
  (let loop ()
    (let ((token (read input-port)))
      (if (constant-token? token)
          (begin
            (write (make-constant-expr token) output-port)
            (newline)))
      (if (not (eq? token #!eof))
          (loop)))))

(parse (current-input-port)
       (current-output-port))
