;;
;; autoffi - c header parser for generating ffi's
;;
;; --- todo -----------------------------------
;;
;; * respect #ifdef __cplusplus type statements if possible
;;

(generate-proper-tail-calls #f)

;; utility
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

(define-macro (assert test . result)
  `(let ((res ,test))
     (if (not ,(if (pair? result)
                   `(equal? res ,(car result))
                   'res))
         (begin
           (parser-error " *** ASSERTION FAILURE *** ")
           (parser-error ',test)
           (parser-error res " != "
                         ,(if (pair? result)
                              (car result)
                              #t))
           (exit 1)))))

(define (split-token done? token)
  (let loop ((acc '())
             (tok token))
    (if (or (null? tok)
            (done? tok))
        (values (reverse acc) tok)
        (loop (cons (car tok) acc)
              (cdr tok)))))

;; numbers
(define (num-token? token)
  (and (pair? token)
       (eq? (car token) 'num)
       (eq? (length token) 2)))

(define (make-num token)
  (cadr token))

;; ids
(define (id-token? token)
  (and (pair? token)
       (eq? (car token) 'id)
       (eq? (length token) 2)))
 
(define (make-id token)
  (string->symbol (cadr token)))

;; pre-processor constants
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


;; type keywords
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
    star
    struct))

(define (type-keyword? token)
  (or (memq token type-keywords)
      (id-token? token)))

(define (read-type-keywords done? token)
  (split-token (lambda (tok)
                 (if (type-keyword? (car tok))
                     (done? tok)
                     #t))
               token))

(assert (type-keyword? 'int))
(assert (type-keyword? 'struct))
(assert (type-keyword? '(id "idgen")))

;; types
(define (type-token? token)
  (and (pair? token)
       (fold (lambda (el r)
               (and (type-keyword? el)
                    r))
             #t
             token)))

(define (symbolify-type token)
  (map (lambda (el)
         (if (id-token? el)
             (make-id el)
             el))
       token))

(define (make-type token)
  (define (symbol-append . args)
    (let ((args (reverse args)))
      (string->symbol
       (fold (lambda (el r)
               (string-append (symbol->string el) "-"
                              r))
             (symbol->string (car args))
             (cdr args)))))
  (let loop ((acc '())
             (pieces (reverse (if (eq? (car token) 'const)
                                  (cdr token)
                                  token))))
    (if (null? pieces)
        (parser-error "invalid type (no concrete type found): " token)
        (if (eq? (car pieces) 'star)
            `(pointer ,(loop acc (cdr pieces)))
            (let ((rest (symbolify-type (reverse pieces))))
              (if (eq? (car rest) 'struct)
                  `(struct ,(symbol->string (make-id (cadr token))))
                  (apply symbol-append rest)))))))

(assert (type-token? '(const unsigned int)))
(assert (type-token? '(const unsigned int err)) #f)
(assert (symbolify-type '(unsigned int)) '(unsigned int))
(assert (symbolify-type '((id "idgen"))) '(idgen))
(assert (make-type '(const unsigned int)) 'unsigned-int)
(assert (make-type '(const idgen)) 'idgen)
(assert (make-type '(unsigned int star star))
        '(pointer (pointer unsigned-int)))
(assert (make-type '(struct (id "idgen")))
        '(struct "idgen"))

;; typedefs
(define (typedef-token? token)
  (and (pair? token)
       (eq? (car token) 'typedef)))

(define (make-typedef-expr token)
  (receive (type-token rest)
      (read-type-keywords (lambda (tok)
                            (eq? (length tok) 1))
                          (cdr token))
    (if (not (type-token? type-token))
        (parser-error "invalid typedef type <" type-token "> in " token)
        (let ((id-token (car rest)))
          (if (not (id-token? id-token))
              (parser-error "invalid typedef id <" id-token
                            "> in " token)
              (let ((id (make-id id-token))
                    (type (make-type type-token)))
                `(c-define-type ,id ,type)))))))

(assert (typedef-token? '(typedef const unsigned int fooint)))
(assert (typedef-token? '(typedef fooint foobar)))
(assert (make-typedef-expr '(typedef const unsigned int (id "fooint")))
        '(c-define-type fooint unsigned-int))
(assert (make-typedef-expr '(typedef struct (id "bar") (id "bar")))
        '(c-define-type bar (struct "bar")))

;; externs
(define (extern-token? token)
  (and (pair? token)
       (eq? (car token) 'extern)))
(assert (extern-token? '(extern unsigned int (id "foo") semicolon)))

;; functions
(define (function-token? token)
  (and (pair? token)
       (receive (type rest)
           (read-type-keywords (lambda (tok)
                                 (eq? (cadr tok) 'open-paren))
                        token)
         (and (type-token? type)
              (id-token? (car rest))
              (> (length rest) 1)
              (eq? (cadr rest) 'open-paren)))))
(assert (function-token? '(unsigned int (id "foo") open-paren close-paren)))

(define (make-function-expr token)
  (receive (ret-type-token token)
      (read-type-keywords (lambda (tok)
                            (and (id-token? (car tok))
                                 (eq? (cadr tok) 'open-paren)))
                          token)
    (let ((id-token (car token))
          (type-tokens (let loop ((acc '())
                                  (tok (cddr token)))
                         (if (or (null? tok)
                                 (eq? (car tok) 'close-paren))
                             (reverse acc)
                             (receive (head tail)
                                 (split-token (lambda (tok)
                                                (or (eq? (car tok) 'comma)
                                                    (eq? (car tok) 'close-paren)))
                                              (if (eq? (car tok) 'comma)
                                                  (cdr tok)
                                                  tok))
                               (let ((num-ids (fold (lambda (el r)
                                                      (if (id-token? el) (+ r 1) r))
                                                    0 head)))
                                 (loop (cons (if (> num-ids 1)
                                                 (reverse (cdr (reverse head)))
                                                 head)
                                             acc)
                                       tail)))))))
      (let ((ret-type (make-type ret-type-token))
            (id (make-id id-token))
            (types (map make-type type-tokens)))
        `(define ,id
           (c-lambda ,(if (equal? types '(void)) '() types)
                     ,ret-type
                     ,(symbol->string id)))))))

(assert (make-function-expr '(unsigned int (id "foo")
                                       open-paren float comma
                                       unsigned int close-paren))
        '(define foo (c-lambda (float unsigned-int) unsigned-int "foo")))

(assert (make-function-expr '(unsigned int (id "foo")
                                       open-paren (id "idgen") star (id "inst")
                                       comma unsigned int close-paren))
        '(define foo (c-lambda ((pointer idgen) unsigned-int) unsigned-int "foo")))

;; parsing
(define (parse input-port output-port)
  (define (maybe-write-expr expr)
    (if expr
        (begin
          (write expr output-port)
          (newline))))
  (let loop ()
    (let ((token (read input-port)))
      (if (not (eq? token #!eof))
          (let ((token (if (extern-token? token)
                           (cdr token)
                           token)))
            (cond
             ((constant-token? token)
              (maybe-write-expr (make-constant-expr token)))
             ((typedef-token? token)
              (maybe-write-expr (make-typedef-expr token)))
             ((function-token? token)
              (maybe-write-expr (make-function-expr token))))
            (loop))))))

(define (headers output-port)
  (write `(c-declare "#include \"gl.h\"") output-port))

(headers (current-output-port))
(parse (current-input-port) (current-output-port))
