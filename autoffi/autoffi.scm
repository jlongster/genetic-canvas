
(include "util.scm")
(include "tokenize.scm")
(include "parse.scm")
(include "analyze.scm")

(define (autoffi file . port)
  (receive (pipe1 pipe2) (make-vector-pipe)
    (let ((src (open-file file)))
      (tokenize src pipe1)
      (parse pipe1 pipe2)
      (analyze pipe2 (or (and (pair? port)
                              (car port))
                         (current-output-port))))))
