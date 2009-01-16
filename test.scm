(namespace ("foo#"))
(##include "~~/lib/gambit#.scm")

(define a 5)

(define (integer->char a) (display a))

(define (eval-it expr)
   (eval expr))

(namespace (""))
