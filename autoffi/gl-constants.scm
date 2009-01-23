
(define (fold kons knil lst)
  (let loop ((acc knil)
             (lst lst))
    (if (null? lst)
        acc
        (loop (kons (car lst) acc)
              (cdr lst)))))

(define (parser-error . args)
  (for-each
   (lambda (x)
     (if (not (string? x))
         (write x (current-error-port))
         (display x (current-error-port))))
   args)
  (newline (current-error-port))
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


;; Primitive types
(define type-keywords
  '(bool
    size_t
    int
    unsigned
    signed
    float
    double
    short
    long
    char
    void
    const
    star))

(define (type-keyword? token)
  (or (memq token type-keywords)
      (and (id-token? token)
           (memq (make-id token) typedefs))))

(define (make-type-keyword token)
  (if (id-token? token)
      (make-id token)
      token))

(define (read-type-keywords token)
  (let loop ((acc '())
             (tok token))
    (if (null? tok)
        (values (reverse acc) '())
        (if (type-keyword? (car tok))
            (loop (cons (make-type-keyword (car tok)) acc)
                  (cdr tok))
            (values (reverse acc) tok)))))

(define (type-token? token)
  (and (pair? token)
       (or (eq? (car token) 'type)
           (and (id-token? token)
                (memq (make-id token) typedefs)))))

(define (make-type token)
  (if (id-token? token)
      (let ((maybe (memq (make-id token) typedefs)))
        (if maybe
            (make-type (car maybe))
            (parser-error "make-type: invalid type token: " token)))
      (string->symbol
       (let ((lst (reverse (cdr token))))
         (fold (lambda (el r)
                 (string-append (symbol->string el) "-"
                                r))
               (symbol->string (car lst))
               (cdr lst))))))

(define typedefs '())

(define (typedef-token? token)
  (and (pair? token)
       (eq? (car token) 'typedef)
       (eq? (length token) 3)))

(define (make-typedef-expr token)
  (let ((type-token (cadr token))
        (id-token (caddr token)))
    (if (not (type-token? type-token))
        (parser-error "invalid typedef type <" type-token "> in " token)
        (if (not (id-token? id-token))
            (parser-error "invalid typedef id <" id-token
                          "> in " token)
            (let ((id (make-id id-token))
                  (type (make-type type-token)))
              `(c-define-type ,id ,type))))))

(define (analyze-typedef token)
  (let ((id-token (caddr token)))
    (if (not (id-token? id-token))
        (parser-error "analyze-typedef: invalid typedef id <" id-token ">")
        (set! typedefs (cons (make-id id-token) typedefs)))))

(define (extern-token? token)
  (and (pair? token)
       (eq? (car token) 'extern)))

(define (function-token? token)
  (and (pair? token)
       (type-token? (car token))
       (id-token? (cadr token))
       (eq? (caddr token) 'open-paren)
       (type-token? (cadddr token))))

(define (make-function-expr token)
  (let ((ret-type-token (car token))
        (id-token (cadr token))
        (type-tokens (reverse
                      (let loop ((acc '())
                                 (parts token))
                        (if (null? parts)
                            acc
                            (case (car parts)
                              ((open-paren comma)
                               (loop (cons (cadr parts) acc)
                                     (cdddr parts)))
                              (else (loop acc (cdr parts)))))))))
    (let ((id (make-id id-token)))
      `(define ,id
         (c-lambda ,type-tokens ,ret-type-token ,(symbol->string id))))))

(define (pre-parse token)
  (define (transformed)
    (let loop ((acc '())
               (token token))
      (cond
       ((null? token)
        (reverse acc))
       ((extern-token? token)
        (loop acc (cdr token)))
       ((pair? (car token))
        (loop (cons (loop '() (car token)) acc)
              (cdr token)))
       (else
        (if (type-keyword? (car token))
            (receive (c-type-token next-token) (read-type-keywords token)
              (loop (cons (cons 'type c-type-token) acc) next-token))
            (loop (cons (car token) acc) (cdr token)))))))

  (let ((token (transformed)))
    (if (typedef-token? token)
        (analyze-typedef token))
    token))

(define (parse input-port output-port)
  (define (maybe-write-expr expr)
    (if expr
        (begin
          (write expr output-port)
          (newline))))
  (let loop ()
    (let ((token (read input-port)))
      (if (not (eq? token #!eof))
          (let ((token (pre-parse token)))
            (cond
             ((constant-token? token)
              (maybe-write-expr (make-constant-expr token)))
             ((typedef-token? token)
              (maybe-write-expr (make-typedef-expr token)))
             ((function-token? token)
              (maybe-write-expr (make-function-expr token))))
            (loop))))))

(parse (current-input-port)
       (current-output-port))
