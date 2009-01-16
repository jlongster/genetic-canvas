(include "autoffi.scm")

;; Token generators depend on these definitions
(define pp-mode #t)
(define (lexer-error c)
  (display "*** ERROR *** invalid token: ")
  (write c)
  (newline)
  (exit 1))

(define (lex-gl output-port)
  (lexer-init 'port (open-file "gl.h"))
  (let loop ()
    (let ((tok (lexer)))
      (write tok output-port)
      (newline)
      (if (not (eq? tok 'stop))
          (loop)))))


(lex-gl (current-output-port))
