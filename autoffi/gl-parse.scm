
(define (parser-error err)
  (display err)
  (exit 1))

(define (parse input-port output-port)
  (define (writer node)
    (write node output-port)
    (newline))
  (let loop ((mode #f) (tokens '()))
    (let ((t (read input-port)))
      (case t
        ((pp-end)
         (if (pair? tokens)
             (writer (reverse tokens))
             (parser-error "invalid preprocessor statement: pp-end"))
         (loop #f '()))
        ((pp-define pp-include pp-if
                    pp-ifdef pp-ifndef
                    pp-else pp-endif
                    pp-undef pp-import
                    pp-pragma pp-error)
         (loop 'pp (list t)))
        ((typedef)
         (loop 'typedef (list t)))
        ((semicolon)
         (if (not (null? tokens))
             (writer (reverse tokens)))
         (loop #f '()))
        ((stop)
         #t)
        (else
         (loop mode (if mode
                        (cons t tokens)
                        tokens)))))))

(parse (current-input-port)
       (current-output-port))
