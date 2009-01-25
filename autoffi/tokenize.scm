(include "autoffi-lexer.scm")

;; Token generators depend on these definitions
(define pp-mode #f)
(define (lexer-error c)
  (display "*** ERROR *** invalid token: ")
  (write c)
  (newline)
  (exit 1))

(define (tokenize input-port output-port)
  (lexer-init 'port input-port)
  (let loop ()
    (let ((tok (lexer)))
      (write tok output-port)
      (newline)
      (if (not (eq? tok 'stop))
          (loop)))))
