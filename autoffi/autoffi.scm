; *** This file starts with a copy of the file multilex.scm ***
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

;
; Gestion des Input Systems
; Fonctions a utiliser par l'usager:
;   lexer-make-IS, lexer-get-func-getc, lexer-get-func-ungetc,
;   lexer-get-func-line, lexer-get-func-column et lexer-get-func-offset
;

; Taille initiale par defaut du buffer d'entree
(define lexer-init-buffer-len 1024)

; Numero du caractere newline
(define lexer-integer-newline (char->integer #\newline))

; Constructeur d'IS brut
(define lexer-raw-IS-maker
  (lambda (buffer read-ptr input-f counters)
    (let ((input-f          input-f)                ; Entree reelle
          (buffer           buffer)                 ; Buffer
          (buflen           (string-length buffer))
          (read-ptr         read-ptr)
          (start-ptr        1)                      ; Marque de debut de lexeme
          (start-line       1)
          (start-column     1)
          (start-offset     0)
          (end-ptr          1)                      ; Marque de fin de lexeme
          (point-ptr        1)                      ; Le point
          (user-ptr         1)                      ; Marque de l'usager
          (user-line        1)
          (user-column      1)
          (user-offset      0)
          (user-up-to-date? #t))                    ; Concerne la colonne seul.
      (letrec
          ((start-go-to-end-none         ; Fonctions de depl. des marques
            (lambda ()
              (set! start-ptr end-ptr)))
           (start-go-to-end-line
            (lambda ()
              (let loop ((ptr start-ptr) (line start-line))
                (if (= ptr end-ptr)
                    (begin
                      (set! start-ptr ptr)
                      (set! start-line line))
                    (if (char=? (string-ref buffer ptr) #\newline)
                        (loop (+ ptr 1) (+ line 1))
                        (loop (+ ptr 1) line))))))
           (start-go-to-end-all
            (lambda ()
              (set! start-offset (+ start-offset (- end-ptr start-ptr)))
              (let loop ((ptr start-ptr)
                         (line start-line)
                         (column start-column))
                (if (= ptr end-ptr)
                    (begin
                      (set! start-ptr ptr)
                      (set! start-line line)
                      (set! start-column column))
                    (if (char=? (string-ref buffer ptr) #\newline)
                        (loop (+ ptr 1) (+ line 1) 1)
                        (loop (+ ptr 1) line (+ column 1)))))))
           (start-go-to-user-none
            (lambda ()
              (set! start-ptr user-ptr)))
           (start-go-to-user-line
            (lambda ()
              (set! start-ptr user-ptr)
              (set! start-line user-line)))
           (start-go-to-user-all
            (lambda ()
              (set! start-line user-line)
              (set! start-offset user-offset)
              (if user-up-to-date?
                  (begin
                    (set! start-ptr user-ptr)
                    (set! start-column user-column))
                  (let loop ((ptr start-ptr) (column start-column))
                    (if (= ptr user-ptr)
                        (begin
                          (set! start-ptr ptr)
                          (set! start-column column))
                        (if (char=? (string-ref buffer ptr) #\newline)
                            (loop (+ ptr 1) 1)
                            (loop (+ ptr 1) (+ column 1))))))))
           (end-go-to-point
            (lambda ()
              (set! end-ptr point-ptr)))
           (point-go-to-start
            (lambda ()
              (set! point-ptr start-ptr)))
           (user-go-to-start-none
            (lambda ()
              (set! user-ptr start-ptr)))
           (user-go-to-start-line
            (lambda ()
              (set! user-ptr start-ptr)
              (set! user-line start-line)))
           (user-go-to-start-all
            (lambda ()
              (set! user-ptr start-ptr)
              (set! user-line start-line)
              (set! user-column start-column)
              (set! user-offset start-offset)
              (set! user-up-to-date? #t)))
           (init-lexeme-none             ; Debute un nouveau lexeme
            (lambda ()
              (if (< start-ptr user-ptr)
                  (start-go-to-user-none))
              (point-go-to-start)))
           (init-lexeme-line
            (lambda ()
              (if (< start-ptr user-ptr)
                  (start-go-to-user-line))
              (point-go-to-start)))
           (init-lexeme-all
            (lambda ()
              (if (< start-ptr user-ptr)
                  (start-go-to-user-all))
              (point-go-to-start)))
           (get-start-line               ; Obtention des stats du debut du lxm
            (lambda ()
              start-line))
           (get-start-column
            (lambda ()
              start-column))
           (get-start-offset
            (lambda ()
              start-offset))
           (peek-left-context            ; Obtention de caracteres (#f si EOF)
            (lambda ()
              (char->integer (string-ref buffer (- start-ptr 1)))))
           (peek-char
            (lambda ()
              (if (< point-ptr read-ptr)
                  (char->integer (string-ref buffer point-ptr))
                  (let ((c (input-f)))
                    (if (char? c)
                        (begin
                          (if (= read-ptr buflen)
                              (reorganize-buffer))
                          (string-set! buffer point-ptr c)
                          (set! read-ptr (+ point-ptr 1))
                          (char->integer c))
                        (begin
                          (set! input-f (lambda () 'eof))
                          #f))))))
           (read-char
            (lambda ()
              (if (< point-ptr read-ptr)
                  (let ((c (string-ref buffer point-ptr)))
                    (set! point-ptr (+ point-ptr 1))
                    (char->integer c))
                  (let ((c (input-f)))
                    (if (char? c)
                        (begin
                          (if (= read-ptr buflen)
                              (reorganize-buffer))
                          (string-set! buffer point-ptr c)
                          (set! read-ptr (+ point-ptr 1))
                          (set! point-ptr read-ptr)
                          (char->integer c))
                        (begin
                          (set! input-f (lambda () 'eof))
                          #f))))))
           (get-start-end-text           ; Obtention du lexeme
            (lambda ()
              (substring buffer start-ptr end-ptr)))
           (get-user-line-line           ; Fonctions pour l'usager
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-line))
              user-line))
           (get-user-line-all
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-all))
              user-line))
           (get-user-column-all
            (lambda ()
              (cond ((< user-ptr start-ptr)
                     (user-go-to-start-all)
                     user-column)
                    (user-up-to-date?
                     user-column)
                    (else
                     (let loop ((ptr start-ptr) (column start-column))
                       (if (= ptr user-ptr)
                           (begin
                             (set! user-column column)
                             (set! user-up-to-date? #t)
                             column)
                           (if (char=? (string-ref buffer ptr) #\newline)
                               (loop (+ ptr 1) 1)
                               (loop (+ ptr 1) (+ column 1)))))))))
           (get-user-offset-all
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-all))
              user-offset))
           (user-getc-none
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-none))
              (if (< user-ptr read-ptr)
                  (let ((c (string-ref buffer user-ptr)))
                    (set! user-ptr (+ user-ptr 1))
                    c)
                  (let ((c (input-f)))
                    (if (char? c)
                        (begin
                          (if (= read-ptr buflen)
                              (reorganize-buffer))
                          (string-set! buffer user-ptr c)
                          (set! read-ptr (+ read-ptr 1))
                          (set! user-ptr read-ptr)
                          c)
                        (begin
                          (set! input-f (lambda () 'eof))
                          'eof))))))
           (user-getc-line
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-line))
              (if (< user-ptr read-ptr)
                  (let ((c (string-ref buffer user-ptr)))
                    (set! user-ptr (+ user-ptr 1))
                    (if (char=? c #\newline)
                        (set! user-line (+ user-line 1)))
                    c)
                  (let ((c (input-f)))
                    (if (char? c)
                        (begin
                          (if (= read-ptr buflen)
                              (reorganize-buffer))
                          (string-set! buffer user-ptr c)
                          (set! read-ptr (+ read-ptr 1))
                          (set! user-ptr read-ptr)
                          (if (char=? c #\newline)
                              (set! user-line (+ user-line 1)))
                          c)
                        (begin
                          (set! input-f (lambda () 'eof))
                          'eof))))))
           (user-getc-all
            (lambda ()
              (if (< user-ptr start-ptr)
                  (user-go-to-start-all))
              (if (< user-ptr read-ptr)
                  (let ((c (string-ref buffer user-ptr)))
                    (set! user-ptr (+ user-ptr 1))
                    (if (char=? c #\newline)
                        (begin
                          (set! user-line (+ user-line 1))
                          (set! user-column 1))
                        (set! user-column (+ user-column 1)))
                    (set! user-offset (+ user-offset 1))
                    c)
                  (let ((c (input-f)))
                    (if (char? c)
                        (begin
                          (if (= read-ptr buflen)
                              (reorganize-buffer))
                          (string-set! buffer user-ptr c)
                          (set! read-ptr (+ read-ptr 1))
                          (set! user-ptr read-ptr)
                          (if (char=? c #\newline)
                              (begin
                                (set! user-line (+ user-line 1))
                                (set! user-column 1))
                              (set! user-column (+ user-column 1)))
                          (set! user-offset (+ user-offset 1))
                          c)
                        (begin
                          (set! input-f (lambda () 'eof))
                          'eof))))))
           (user-ungetc-none
            (lambda ()
              (if (> user-ptr start-ptr)
                  (set! user-ptr (- user-ptr 1)))))
           (user-ungetc-line
            (lambda ()
              (if (> user-ptr start-ptr)
                  (begin
                    (set! user-ptr (- user-ptr 1))
                    (let ((c (string-ref buffer user-ptr)))
                      (if (char=? c #\newline)
                          (set! user-line (- user-line 1))))))))
           (user-ungetc-all
            (lambda ()
              (if (> user-ptr start-ptr)
                  (begin
                    (set! user-ptr (- user-ptr 1))
                    (let ((c (string-ref buffer user-ptr)))
                      (if (char=? c #\newline)
                          (begin
                            (set! user-line (- user-line 1))
                            (set! user-up-to-date? #f))
                          (set! user-column (- user-column 1)))
                      (set! user-offset (- user-offset 1)))))))
           (reorganize-buffer            ; Decaler ou agrandir le buffer
            (lambda ()
              (if (< (* 2 start-ptr) buflen)
                  (let* ((newlen (* 2 buflen))
                         (newbuf (make-string newlen))
                         (delta (- start-ptr 1)))
                    (let loop ((from (- start-ptr 1)))
                      (if (< from buflen)
                          (begin
                            (string-set! newbuf
                                         (- from delta)
                                         (string-ref buffer from))
                            (loop (+ from 1)))))
                    (set! buffer    newbuf)
                    (set! buflen    newlen)
                    (set! read-ptr  (- read-ptr delta))
                    (set! start-ptr (- start-ptr delta))
                    (set! end-ptr   (- end-ptr delta))
                    (set! point-ptr (- point-ptr delta))
                    (set! user-ptr  (- user-ptr delta)))
                  (let ((delta (- start-ptr 1)))
                    (let loop ((from (- start-ptr 1)))
                      (if (< from buflen)
                          (begin
                            (string-set! buffer
                                         (- from delta)
                                         (string-ref buffer from))
                            (loop (+ from 1)))))
                    (set! read-ptr  (- read-ptr delta))
                    (set! start-ptr (- start-ptr delta))
                    (set! end-ptr   (- end-ptr delta))
                    (set! point-ptr (- point-ptr delta))
                    (set! user-ptr  (- user-ptr delta)))))))
        (list (cons 'start-go-to-end
                    (cond ((eq? counters 'none) start-go-to-end-none)
                          ((eq? counters 'line) start-go-to-end-line)
                          ((eq? counters 'all ) start-go-to-end-all)))
              (cons 'end-go-to-point
                    end-go-to-point)
              (cons 'init-lexeme
                    (cond ((eq? counters 'none) init-lexeme-none)
                          ((eq? counters 'line) init-lexeme-line)
                          ((eq? counters 'all ) init-lexeme-all)))
              (cons 'get-start-line
                    get-start-line)
              (cons 'get-start-column
                    get-start-column)
              (cons 'get-start-offset
                    get-start-offset)
              (cons 'peek-left-context
                    peek-left-context)
              (cons 'peek-char
                    peek-char)
              (cons 'read-char
                    read-char)
              (cons 'get-start-end-text
                    get-start-end-text)
              (cons 'get-user-line
                    (cond ((eq? counters 'none) #f)
                          ((eq? counters 'line) get-user-line-line)
                          ((eq? counters 'all ) get-user-line-all)))
              (cons 'get-user-column
                    (cond ((eq? counters 'none) #f)
                          ((eq? counters 'line) #f)
                          ((eq? counters 'all ) get-user-column-all)))
              (cons 'get-user-offset
                    (cond ((eq? counters 'none) #f)
                          ((eq? counters 'line) #f)
                          ((eq? counters 'all ) get-user-offset-all)))
              (cons 'user-getc
                    (cond ((eq? counters 'none) user-getc-none)
                          ((eq? counters 'line) user-getc-line)
                          ((eq? counters 'all ) user-getc-all)))
              (cons 'user-ungetc
                    (cond ((eq? counters 'none) user-ungetc-none)
                          ((eq? counters 'line) user-ungetc-line)
                          ((eq? counters 'all ) user-ungetc-all))))))))

; Construit un Input System
; Le premier parametre doit etre parmi "port", "procedure" ou "string"
; Prend un parametre facultatif qui doit etre parmi
; "none", "line" ou "all"
(define lexer-make-IS
  (lambda (input-type input . largs)
    (let ((counters-type (cond ((null? largs)
                                'line)
                               ((memq (car largs) '(none line all))
                                (car largs))
                               (else
                                'line))))
      (cond ((and (eq? input-type 'port) (input-port? input))
             (let* ((buffer   (make-string lexer-init-buffer-len #\newline))
                    (read-ptr 1)
                    (input-f  (lambda () (read-char input))))
               (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
            ((and (eq? input-type 'procedure) (procedure? input))
             (let* ((buffer   (make-string lexer-init-buffer-len #\newline))
                    (read-ptr 1)
                    (input-f  input))
               (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
            ((and (eq? input-type 'string) (string? input))
             (let* ((buffer   (string-append (string #\newline) input))
                    (read-ptr (string-length buffer))
                    (input-f  (lambda () 'eof)))
               (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
            (else
             (let* ((buffer   (string #\newline))
                    (read-ptr 1)
                    (input-f  (lambda () 'eof)))
               (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))))))

; Les fonctions:
;   lexer-get-func-getc, lexer-get-func-ungetc,
;   lexer-get-func-line, lexer-get-func-column et lexer-get-func-offset
(define lexer-get-func-getc
  (lambda (IS) (cdr (assq 'user-getc IS))))
(define lexer-get-func-ungetc
  (lambda (IS) (cdr (assq 'user-ungetc IS))))
(define lexer-get-func-line
  (lambda (IS) (cdr (assq 'get-user-line IS))))
(define lexer-get-func-column
  (lambda (IS) (cdr (assq 'get-user-column IS))))
(define lexer-get-func-offset
  (lambda (IS) (cdr (assq 'get-user-offset IS))))

;
; Gestion des lexers
;

; Fabrication de lexer a partir d'arbres de decision
(define lexer-make-tree-lexer
  (lambda (tables IS)
    (letrec
        (; Contenu de la table
         (counters-type        (vector-ref tables 0))
         (<<EOF>>-pre-action   (vector-ref tables 1))
         (<<ERROR>>-pre-action (vector-ref tables 2))
         (rules-pre-actions    (vector-ref tables 3))
         (table-nl-start       (vector-ref tables 5))
         (table-no-nl-start    (vector-ref tables 6))
         (trees-v              (vector-ref tables 7))
         (acc-v                (vector-ref tables 8))

         ; Contenu du IS
         (IS-start-go-to-end    (cdr (assq 'start-go-to-end IS)))
         (IS-end-go-to-point    (cdr (assq 'end-go-to-point IS)))
         (IS-init-lexeme        (cdr (assq 'init-lexeme IS)))
         (IS-get-start-line     (cdr (assq 'get-start-line IS)))
         (IS-get-start-column   (cdr (assq 'get-start-column IS)))
         (IS-get-start-offset   (cdr (assq 'get-start-offset IS)))
         (IS-peek-left-context  (cdr (assq 'peek-left-context IS)))
         (IS-peek-char          (cdr (assq 'peek-char IS)))
         (IS-read-char          (cdr (assq 'read-char IS)))
         (IS-get-start-end-text (cdr (assq 'get-start-end-text IS)))
         (IS-get-user-line      (cdr (assq 'get-user-line IS)))
         (IS-get-user-column    (cdr (assq 'get-user-column IS)))
         (IS-get-user-offset    (cdr (assq 'get-user-offset IS)))
         (IS-user-getc          (cdr (assq 'user-getc IS)))
         (IS-user-ungetc        (cdr (assq 'user-ungetc IS)))

         ; Resultats
         (<<EOF>>-action   #f)
         (<<ERROR>>-action #f)
         (rules-actions    #f)
         (states           #f)
         (final-lexer      #f)

         ; Gestion des hooks
         (hook-list '())
         (add-hook
          (lambda (thunk)
            (set! hook-list (cons thunk hook-list))))
         (apply-hooks
          (lambda ()
            (let loop ((l hook-list))
              (if (pair? l)
                  (begin
                    ((car l))
                    (loop (cdr l)))))))

         ; Preparation des actions
         (set-action-statics
          (lambda (pre-action)
            (pre-action final-lexer IS-user-getc IS-user-ungetc)))
         (prepare-special-action-none
          (lambda (pre-action)
            (let ((action #f))
              (let ((result
                     (lambda ()
                       (action "")))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-special-action-line
          (lambda (pre-action)
            (let ((action #f))
              (let ((result
                     (lambda (yyline)
                       (action "" yyline)))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-special-action-all
          (lambda (pre-action)
            (let ((action #f))
              (let ((result
                     (lambda (yyline yycolumn yyoffset)
                       (action "" yyline yycolumn yyoffset)))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-special-action
          (lambda (pre-action)
            (cond ((eq? counters-type 'none)
                   (prepare-special-action-none pre-action))
                  ((eq? counters-type 'line)
                   (prepare-special-action-line pre-action))
                  ((eq? counters-type 'all)
                   (prepare-special-action-all  pre-action)))))
         (prepare-action-yytext-none
          (lambda (pre-action)
            (let ((get-start-end-text IS-get-start-end-text)
                  (start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda ()
                       (let ((yytext (get-start-end-text)))
                         (start-go-to-end)
                         (action yytext))))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-yytext-line
          (lambda (pre-action)
            (let ((get-start-end-text IS-get-start-end-text)
                  (start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda (yyline)
                       (let ((yytext (get-start-end-text)))
                         (start-go-to-end)
                         (action yytext yyline))))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-yytext-all
          (lambda (pre-action)
            (let ((get-start-end-text IS-get-start-end-text)
                  (start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda (yyline yycolumn yyoffset)
                       (let ((yytext (get-start-end-text)))
                         (start-go-to-end)
                         (action yytext yyline yycolumn yyoffset))))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-yytext
          (lambda (pre-action)
            (cond ((eq? counters-type 'none)
                   (prepare-action-yytext-none pre-action))
                  ((eq? counters-type 'line)
                   (prepare-action-yytext-line pre-action))
                  ((eq? counters-type 'all)
                   (prepare-action-yytext-all  pre-action)))))
         (prepare-action-no-yytext-none
          (lambda (pre-action)
            (let ((start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda ()
                       (start-go-to-end)
                       (action)))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-no-yytext-line
          (lambda (pre-action)
            (let ((start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda (yyline)
                       (start-go-to-end)
                       (action yyline)))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-no-yytext-all
          (lambda (pre-action)
            (let ((start-go-to-end    IS-start-go-to-end)
                  (action             #f))
              (let ((result
                     (lambda (yyline yycolumn yyoffset)
                       (start-go-to-end)
                       (action yyline yycolumn yyoffset)))
                    (hook
                     (lambda ()
                       (set! action (set-action-statics pre-action)))))
                (add-hook hook)
                result))))
         (prepare-action-no-yytext
          (lambda (pre-action)
            (cond ((eq? counters-type 'none)
                   (prepare-action-no-yytext-none pre-action))
                  ((eq? counters-type 'line)
                   (prepare-action-no-yytext-line pre-action))
                  ((eq? counters-type 'all)
                   (prepare-action-no-yytext-all  pre-action)))))

         ; Fabrique les fonctions de dispatch
         (prepare-dispatch-err
          (lambda (leaf)
            (lambda (c)
              #f)))
         (prepare-dispatch-number
          (lambda (leaf)
            (let ((state-function #f))
              (let ((result
                     (lambda (c)
                       state-function))
                    (hook
                     (lambda ()
                       (set! state-function (vector-ref states leaf)))))
                (add-hook hook)
                result))))
         (prepare-dispatch-leaf
          (lambda (leaf)
            (if (eq? leaf 'err)
                (prepare-dispatch-err leaf)
                (prepare-dispatch-number leaf))))
         (prepare-dispatch-<
          (lambda (tree)
            (let ((left-tree  (list-ref tree 1))
                  (right-tree (list-ref tree 2)))
              (let ((bound      (list-ref tree 0))
                    (left-func  (prepare-dispatch-tree left-tree))
                    (right-func (prepare-dispatch-tree right-tree)))
                (lambda (c)
                  (if (< c bound)
                      (left-func c)
                      (right-func c)))))))
         (prepare-dispatch-=
          (lambda (tree)
            (let ((left-tree  (list-ref tree 2))
                  (right-tree (list-ref tree 3)))
              (let ((bound      (list-ref tree 1))
                    (left-func  (prepare-dispatch-tree left-tree))
                    (right-func (prepare-dispatch-tree right-tree)))
                (lambda (c)
                  (if (= c bound)
                      (left-func c)
                      (right-func c)))))))
         (prepare-dispatch-tree
          (lambda (tree)
            (cond ((not (pair? tree))
                   (prepare-dispatch-leaf tree))
                  ((eq? (car tree) '=)
                   (prepare-dispatch-= tree))
                  (else
                   (prepare-dispatch-< tree)))))
         (prepare-dispatch
          (lambda (tree)
            (let ((dicho-func (prepare-dispatch-tree tree)))
              (lambda (c)
                (and c (dicho-func c))))))

         ; Fabrique les fonctions de transition (read & go) et (abort)
         (prepare-read-n-go
          (lambda (tree)
            (let ((dispatch-func (prepare-dispatch tree))
                  (read-char     IS-read-char))
              (lambda ()
                (dispatch-func (read-char))))))
         (prepare-abort
          (lambda (tree)
            (lambda ()
              #f)))
         (prepare-transition
          (lambda (tree)
            (if (eq? tree 'err)
                (prepare-abort     tree)
                (prepare-read-n-go tree))))

         ; Fabrique les fonctions d'etats ([set-end] & trans)
         (prepare-state-no-acc
           (lambda (s r1 r2)
             (let ((trans-func (prepare-transition (vector-ref trees-v s))))
               (lambda (action)
                 (let ((next-state (trans-func)))
                   (if next-state
                       (next-state action)
                       action))))))
         (prepare-state-yes-no
          (lambda (s r1 r2)
            (let ((peek-char       IS-peek-char)
                  (end-go-to-point IS-end-go-to-point)
                  (new-action1     #f)
                  (trans-func (prepare-transition (vector-ref trees-v s))))
              (let ((result
                     (lambda (action)
                       (let* ((c (peek-char))
                              (new-action
                               (if (or (not c) (= c lexer-integer-newline))
                                   (begin
                                     (end-go-to-point)
                                     new-action1)
                                   action))
                              (next-state (trans-func)))
                         (if next-state
                             (next-state new-action)
                             new-action))))
                    (hook
                     (lambda ()
                       (set! new-action1 (vector-ref rules-actions r1)))))
                (add-hook hook)
                result))))
         (prepare-state-diff-acc
          (lambda (s r1 r2)
            (let ((end-go-to-point IS-end-go-to-point)
                  (peek-char       IS-peek-char)
                  (new-action1     #f)
                  (new-action2     #f)
                  (trans-func (prepare-transition (vector-ref trees-v s))))
              (let ((result
                     (lambda (action)
                       (end-go-to-point)
                       (let* ((c (peek-char))
                              (new-action
                               (if (or (not c) (= c lexer-integer-newline))
                                   new-action1
                                   new-action2))
                              (next-state (trans-func)))
                         (if next-state
                             (next-state new-action)
                             new-action))))
                    (hook
                     (lambda ()
                       (set! new-action1 (vector-ref rules-actions r1))
                       (set! new-action2 (vector-ref rules-actions r2)))))
                (add-hook hook)
                result))))
         (prepare-state-same-acc
          (lambda (s r1 r2)
            (let ((end-go-to-point IS-end-go-to-point)
                  (trans-func (prepare-transition (vector-ref trees-v s)))
                  (new-action #f))
              (let ((result
                     (lambda (action)
                       (end-go-to-point)
                       (let ((next-state (trans-func)))
                         (if next-state
                             (next-state new-action)
                             new-action))))
                    (hook
                     (lambda ()
                       (set! new-action (vector-ref rules-actions r1)))))
                (add-hook hook)
                result))))
         (prepare-state
          (lambda (s)
            (let* ((acc (vector-ref acc-v s))
                   (r1 (car acc))
                   (r2 (cdr acc)))
              (cond ((not r1)  (prepare-state-no-acc   s r1 r2))
                    ((not r2)  (prepare-state-yes-no   s r1 r2))
                    ((< r1 r2) (prepare-state-diff-acc s r1 r2))
                    (else      (prepare-state-same-acc s r1 r2))))))

         ; Fabrique la fonction de lancement du lexage a l'etat de depart
         (prepare-start-same
          (lambda (s1 s2)
            (let ((peek-char    IS-peek-char)
                  (eof-action   #f)
                  (start-state  #f)
                  (error-action #f))
              (let ((result
                     (lambda ()
                       (if (not (peek-char))
                           eof-action
                           (start-state error-action))))
                    (hook
                     (lambda ()
                       (set! eof-action   <<EOF>>-action)
                       (set! start-state  (vector-ref states s1))
                       (set! error-action <<ERROR>>-action))))
                (add-hook hook)
                result))))
         (prepare-start-diff
          (lambda (s1 s2)
            (let ((peek-char         IS-peek-char)
                  (eof-action        #f)
                  (peek-left-context IS-peek-left-context)
                  (start-state1      #f)
                  (start-state2      #f)
                  (error-action      #f))
              (let ((result
                     (lambda ()
                       (cond ((not (peek-char))
                              eof-action)
                             ((= (peek-left-context) lexer-integer-newline)
                              (start-state1 error-action))
                             (else
                              (start-state2 error-action)))))
                    (hook
                     (lambda ()
                       (set! eof-action <<EOF>>-action)
                       (set! start-state1 (vector-ref states s1))
                       (set! start-state2 (vector-ref states s2))
                       (set! error-action <<ERROR>>-action))))
                (add-hook hook)
                result))))
         (prepare-start
          (lambda ()
            (let ((s1 table-nl-start)
                  (s2 table-no-nl-start))
              (if (= s1 s2)
                  (prepare-start-same s1 s2)
                  (prepare-start-diff s1 s2)))))

         ; Fabrique la fonction principale
         (prepare-lexer-none
          (lambda ()
            (let ((init-lexeme IS-init-lexeme)
                  (start-func  (prepare-start)))
              (lambda ()
                (init-lexeme)
                ((start-func))))))
         (prepare-lexer-line
          (lambda ()
            (let ((init-lexeme    IS-init-lexeme)
                  (get-start-line IS-get-start-line)
                  (start-func     (prepare-start)))
              (lambda ()
                (init-lexeme)
                (let ((yyline (get-start-line)))
                  ((start-func) yyline))))))
         (prepare-lexer-all
          (lambda ()
            (let ((init-lexeme      IS-init-lexeme)
                  (get-start-line   IS-get-start-line)
                  (get-start-column IS-get-start-column)
                  (get-start-offset IS-get-start-offset)
                  (start-func       (prepare-start)))
              (lambda ()
                (init-lexeme)
                (let ((yyline   (get-start-line))
                      (yycolumn (get-start-column))
                      (yyoffset (get-start-offset)))
                  ((start-func) yyline yycolumn yyoffset))))))
         (prepare-lexer
          (lambda ()
            (cond ((eq? counters-type 'none) (prepare-lexer-none))
                  ((eq? counters-type 'line) (prepare-lexer-line))
                  ((eq? counters-type 'all)  (prepare-lexer-all))))))

      ; Calculer la valeur de <<EOF>>-action et de <<ERROR>>-action
      (set! <<EOF>>-action   (prepare-special-action <<EOF>>-pre-action))
      (set! <<ERROR>>-action (prepare-special-action <<ERROR>>-pre-action))

      ; Calculer la valeur de rules-actions
      (let* ((len (quotient (vector-length rules-pre-actions) 2))
             (v (make-vector len)))
        (let loop ((r (- len 1)))
          (if (< r 0)
              (set! rules-actions v)
              (let* ((yytext? (vector-ref rules-pre-actions (* 2 r)))
                     (pre-action (vector-ref rules-pre-actions (+ (* 2 r) 1)))
                     (action (if yytext?
                                 (prepare-action-yytext    pre-action)
                                 (prepare-action-no-yytext pre-action))))
                (vector-set! v r action)
                (loop (- r 1))))))

      ; Calculer la valeur de states
      (let* ((len (vector-length trees-v))
             (v (make-vector len)))
        (let loop ((s (- len 1)))
          (if (< s 0)
              (set! states v)
              (begin
                (vector-set! v s (prepare-state s))
                (loop (- s 1))))))

      ; Calculer la valeur de final-lexer
      (set! final-lexer (prepare-lexer))

      ; Executer les hooks
      (apply-hooks)

      ; Resultat
      final-lexer)))

; Fabrication de lexer a partir de listes de caracteres taggees
(define lexer-make-char-lexer
  (let* ((char->class
          (lambda (c)
            (let ((n (char->integer c)))
              (list (cons n n)))))
         (merge-sort
          (lambda (l combine zero-elt)
            (if (null? l)
                zero-elt
                (let loop1 ((l l))
                  (if (null? (cdr l))
                      (car l)
                      (loop1
                       (let loop2 ((l l))
                         (cond ((null? l)
                                l)
                               ((null? (cdr l))
                                l)
                               (else
                                (cons (combine (car l) (cadr l))
                                      (loop2 (cddr l))))))))))))
         (finite-class-union
          (lambda (c1 c2)
            (let loop ((c1 c1) (c2 c2) (u '()))
              (if (null? c1)
                  (if (null? c2)
                      (reverse u)
                      (loop c1 (cdr c2) (cons (car c2) u)))
                  (if (null? c2)
                      (loop (cdr c1) c2 (cons (car c1) u))
                      (let* ((r1 (car c1))
                             (r2 (car c2))
                             (r1start (car r1))
                             (r1end (cdr r1))
                             (r2start (car r2))
                             (r2end (cdr r2)))
                        (if (<= r1start r2start)
                            (cond ((< (+ r1end 1) r2start)
                                   (loop (cdr c1) c2 (cons r1 u)))
                                  ((<= r1end r2end)
                                   (loop (cdr c1)
                                         (cons (cons r1start r2end) (cdr c2))
                                         u))
                                  (else
                                   (loop c1 (cdr c2) u)))
                            (cond ((> r1start (+ r2end 1))
                                   (loop c1 (cdr c2) (cons r2 u)))
                                  ((>= r1end r2end)
                                   (loop (cons (cons r2start r1end) (cdr c1))
                                         (cdr c2)
                                         u))
                                  (else
                                   (loop (cdr c1) c2 u))))))))))
         (char-list->class
          (lambda (cl)
            (let ((classes (map char->class cl)))
              (merge-sort classes finite-class-union '()))))
         (class-<
          (lambda (b1 b2)
            (cond ((eq? b1 'inf+) #f)
                  ((eq? b2 'inf-) #f)
                  ((eq? b1 'inf-) #t)
                  ((eq? b2 'inf+) #t)
                  (else (< b1 b2)))))
         (finite-class-compl
          (lambda (c)
            (let loop ((c c) (start 'inf-))
              (if (null? c)
                  (list (cons start 'inf+))
                  (let* ((r (car c))
                         (rstart (car r))
                         (rend (cdr r)))
                    (if (class-< start rstart)
                        (cons (cons start (- rstart 1))
                              (loop c rstart))
                        (loop (cdr c) (+ rend 1))))))))
         (tagged-chars->class
          (lambda (tcl)
            (let* ((inverse? (car tcl))
                   (cl (cdr tcl))
                   (class-tmp (char-list->class cl)))
              (if inverse? (finite-class-compl class-tmp) class-tmp))))
         (charc->arc
          (lambda (charc)
            (let* ((tcl (car charc))
                   (dest (cdr charc))
                   (class (tagged-chars->class tcl)))
              (cons class dest))))
         (arc->sharcs
          (lambda (arc)
            (let* ((range-l (car arc))
                   (dest (cdr arc))
                   (op (lambda (range) (cons range dest))))
              (map op range-l))))
         (class-<=
          (lambda (b1 b2)
            (cond ((eq? b1 'inf-) #t)
                  ((eq? b2 'inf+) #t)
                  ((eq? b1 'inf+) #f)
                  ((eq? b2 'inf-) #f)
                  (else (<= b1 b2)))))
         (sharc-<=
          (lambda (sharc1 sharc2)
            (class-<= (caar sharc1) (caar sharc2))))
         (merge-sharcs
          (lambda (l1 l2)
            (let loop ((l1 l1) (l2 l2))
              (cond ((null? l1)
                     l2)
                    ((null? l2)
                     l1)
                    (else
                     (let ((sharc1 (car l1))
                           (sharc2 (car l2)))
                       (if (sharc-<= sharc1 sharc2)
                           (cons sharc1 (loop (cdr l1) l2))
                           (cons sharc2 (loop l1 (cdr l2))))))))))
         (class-= eqv?)
         (fill-error
          (lambda (sharcs)
            (let loop ((sharcs sharcs) (start 'inf-))
              (cond ((class-= start 'inf+)
                     '())
                    ((null? sharcs)
                     (cons (cons (cons start 'inf+) 'err)
                           (loop sharcs 'inf+)))
                    (else
                     (let* ((sharc (car sharcs))
                            (h (caar sharc))
                            (t (cdar sharc)))
                       (if (class-< start h)
                           (cons (cons (cons start (- h 1)) 'err)
                                 (loop sharcs h))
                           (cons sharc (loop (cdr sharcs)
                                             (if (class-= t 'inf+)
                                                 'inf+
                                                 (+ t 1)))))))))))
         (charcs->tree
          (lambda (charcs)
            (let* ((op (lambda (charc) (arc->sharcs (charc->arc charc))))
                   (sharcs-l (map op charcs))
                   (sorted-sharcs (merge-sort sharcs-l merge-sharcs '()))
                   (full-sharcs (fill-error sorted-sharcs))
                   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
                   (table (list->vector (map op full-sharcs))))
              (let loop ((left 0) (right (- (vector-length table) 1)))
                (if (= left right)
                    (cdr (vector-ref table left))
                    (let ((mid (quotient (+ left right 1) 2)))
                      (if (and (= (+ left 2) right)
                               (= (+ (car (vector-ref table mid)) 1)
                                  (car (vector-ref table right)))
                               (eqv? (cdr (vector-ref table left))
                                     (cdr (vector-ref table right))))
                          (list '=
                                (car (vector-ref table mid))
                                (cdr (vector-ref table mid))
                                (cdr (vector-ref table left)))
                          (list (car (vector-ref table mid))
                                (loop left (- mid 1))
                                (loop mid right))))))))))
    (lambda (tables IS)
      (let ((counters         (vector-ref tables 0))
            (<<EOF>>-action   (vector-ref tables 1))
            (<<ERROR>>-action (vector-ref tables 2))
            (rules-actions    (vector-ref tables 3))
            (nl-start         (vector-ref tables 5))
            (no-nl-start      (vector-ref tables 6))
            (charcs-v         (vector-ref tables 7))
            (acc-v            (vector-ref tables 8)))
        (let* ((len (vector-length charcs-v))
               (v (make-vector len)))
          (let loop ((i (- len 1)))
            (if (>= i 0)
                (begin
                  (vector-set! v i (charcs->tree (vector-ref charcs-v i)))
                  (loop (- i 1)))
                (lexer-make-tree-lexer
                 (vector counters
                         <<EOF>>-action
                         <<ERROR>>-action
                         rules-actions
                         'decision-trees
                         nl-start
                         no-nl-start
                         v
                         acc-v)
                 IS))))))))

; Fabrication d'un lexer a partir de code pre-genere
(define lexer-make-code-lexer
  (lambda (tables IS)
    (let ((<<EOF>>-pre-action   (vector-ref tables 1))
          (<<ERROR>>-pre-action (vector-ref tables 2))
          (rules-pre-action     (vector-ref tables 3))
          (code                 (vector-ref tables 5)))
      (code <<EOF>>-pre-action <<ERROR>>-pre-action rules-pre-action IS))))

(define lexer-make-lexer
  (lambda (tables IS)
    (let ((automaton-type (vector-ref tables 4)))
      (cond ((eq? automaton-type 'decision-trees)
             (lexer-make-tree-lexer tables IS))
            ((eq? automaton-type 'tagged-chars-lists)
             (lexer-make-char-lexer tables IS))
            ((eq? automaton-type 'code)
             (lexer-make-code-lexer tables IS))))))

;
; Table generated from the file autoffi.l by SILex 1.0
;

(define lexer-default-table
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
                                   (begin (set! pp-mode #f) 'stop)
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
                                   (lexer-error (yygetc))
       ))
   (vector
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (if pp-mode
                                       (begin 
                                         (set! pp-mode #f) 'pp-end)
                                       (yycontinue) )
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (yycontinue)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (let loop ()
                                     (let ([c (yygetc)])
                                       (if (or (eq? 'eof c) (char=? #\newline c))
                                           (begin
                                             (if pp-mode
                                                 (begin
                                                   (set! pp-mode #f)
                                                   'pp-end)
                                                 (yycontinue) ) )
                                           (loop) ) ) )
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (let loop ([c (yygetc)])
                                     (cond [(eq? 'eof c) (parsing-error "unexpected end of comment")]
                                           [(char=? #\newline c) (loop (yygetc))]
                                           [(char=? c #\*)
                                            (let ([c2 (yygetc)])
                                              (if (eq? #\/ c2)
                                                  (yycontinue)
                                                  (loop c2) ) ) ]
                                           [else (loop (yygetc))] ) )
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'enum
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'typedef
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'extern
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'static
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'bool
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'size_t
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'int
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'unsigned
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'signed
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'float
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'double
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'short
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'long
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'char
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'void
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'struct
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'union
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'const
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'class
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'public
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'protected
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'private
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'volatile
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'namespace
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'virtual
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'explicit
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'inline
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'using
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'interface
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'implementation
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'end
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'objc-class
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'protocol
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'objc-public
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'objc-protected
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'objc-private
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (list 'id "@encode")
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (list 'id "@defs")
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (list 'id "@selector")
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'dots
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode 'define) 'pp-define)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode 'include) 'pp-include)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode 'import) 'pp-import)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-ifdef)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-ifndef)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-elif)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-if)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-else)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-pragma)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-endif)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-error)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                          (begin (set! pp-mode #t) 'pp-undef)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(op "#")
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'if
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'else
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (let loop ([cs '()])
                                     (let ([c (yygetc)])
                                       (cond [(eq? 'eof c)
                                              (parsing-error "unexpected end of string constant")]
                                             [(char=? c #\\) (loop (cons (yygetc) cs))]
                                             [(char=? c #\")
                                              (list 'string (list->string (reverse cs))) ]
                                             [else (loop (cons c cs))] ) ) )
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'char (string->number (substring yytext 3 5) 8))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\nul)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\alarm)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\backspace)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\page)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\newline)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\return)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\tab)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   '(char #\vtab)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'char (string-ref yytext 2))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'char (string-ref yytext 1))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'id yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'num (string->number (substring yytext 2 (string-length yytext)) 16))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                   (list 'num (string->number (substring yytext 1 (string-length yytext)) 8))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                     (list 'num (string->number yytext))
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   (if (eq? pp-mode 'include)
                                        (let loop ([s '()])
                                          (let ([c (yygetc)])
                                           (cond [(eq? 'eof c) (parsing-error "unexpected end of include file name")]
                                                 [(char=? #\> c)
                                                  (set! pp-mode #f)
                                                  `(i-string ,(list->string (reverse s))) ]
                                                 [else (loop (cons c s))] ) ) ) 
                                        `(op "<") )
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'open-paren
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'close-paren
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'open-bracket
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'close-bracket
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'open-curly
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'close-curly
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'comma
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'semicolon
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                                   'star
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                     (list 'op yytext)
        )))
   'decision-trees
   0
   1
   '#((65 (41 (33 (12 (10 (9 err 46) (11 48 err)) (14 (13 45 49) (32 err
    47))) (37 (35 (34 2 27) (36 28 err)) (39 (38 9 8) (40 26 19)))) (49 (45
    (43 (42 18 11) (44 23 13)) (47 (46 22 29) (48 44 24))) (61 (59 (58 21
    4) (60 12 20)) (63 (62 3 6) (64 10 30))))) (108 (98 (94 (92 (91 25 17)
    (93 50 16)) (96 (95 5 25) (97 err 25))) (102 (100 (99 40 34) (101 36
    43)) (105 (103 37 25) (106 39 25)))) (117 (112 (110 (109 35 25) (111 31
    25)) (115 (113 32 25) (116 41 42))) (124 (119 (118 38 33) (123 25 15))
    (126 (125 7 14) (127 10 err)))))) (65 (41 (33 (12 (10 (9 err 46) (11 48
    err)) (14 (13 45 49) (32 err 53))) (37 (35 (34 2 27) (36 51 err)) (39
    (38 9 8) (40 26 19)))) (49 (45 (43 (42 18 11) (44 23 13)) (47 (46 22
    29) (48 44 24))) (61 (59 (58 21 4) (60 12 20)) (63 (62 3 6) (64 10
    30))))) (108 (98 (94 (92 (91 25 17) (93 50 16)) (96 (95 5 25) (97 err
    25))) (102 (100 (99 40 34) (101 36 43)) (105 (103 37 25) (106 39 25))))
    (117 (112 (110 (109 35 25) (111 31 25)) (115 (113 32 25) (116 41 52)))
    (124 (119 (118 38 33) (123 25 15)) (126 (125 7 14) (127 10 err)))))) (=
    61 10 err) (= 61 10 err) (= 58 10 err) (= 61 10 err) (62 (61 err 10)
    (63 54 err)) (62 (61 err 10) (= 124 10 err)) (39 (38 err 10) (= 61 10
    err)) (= 61 10 err) err (= 61 10 err) err err err err err err err err
    (61 (60 err 55) (62 10 err)) (58 (47 (46 err 57) (48 err 21)) (70 (69
    err 56) (= 101 56 err))) (58 (48 err 21) (= 61 10 err)) (58 (48 err 21)
    (= 61 10 err)) (70 (48 (= 46 57 err) (58 (56 58 21) (69 err 56))) (101
    (= 88 59 err) (120 (102 56 err) (121 59 err)))) (91 (58 (48 err 25) (65
    err 25)) (96 (95 err 25) (97 err (123 25 err)))) (11 (10 60 err) (= 92
    61 60)) err (105 (100 (= 32 66 err) (101 67 (102 64 err))) (113 (106 65
    (112 err 63)) (= 117 62 err))) (= 46 68 err) (102 (99 (= 34 27 err)
    (100 72 (101 70 73))) (112 (= 105 74 err) (115 (113 71 err) (116 69
    err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (97 (96 25 err) (98
    75 (123 25 err)))) (96 (65 (48 err (58 25 err)) (91 25 (95 err 25)))
    (115 (97 err (114 25 76)) (118 (117 25 77) (123 25 err)))) (96 (65 (48
    err (58 25 err)) (91 25 (95 err 25))) (106 (97 err (105 25 78)) (112
    (111 25 79) (123 25 err)))) (97 (65 (48 err (58 25 err)) (95 (91 25
    err) (96 25 err))) (109 (105 (104 25 82) (108 25 80)) (112 (111 25 81)
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (111 (= 96
    err 25) (112 83 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (111 (= 96 err 25) (112 84 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (108 (= 96 err 25) (109 85 (123 25 err)))) (96
    (65 (48 err (58 25 err)) (91 25 (95 err 25))) (111 (97 err (110 25 87))
    (116 (115 25 86) (123 25 err)))) (96 (65 (48 err (58 25 err)) (91 25
    (95 err 25))) (103 (97 err (102 25 88)) (111 (110 25 89) (123 25
    err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (111 (= 96 err 25)
    (112 90 (123 25 err)))) (97 (65 (48 err (58 25 err)) (95 (91 25 err)
    (96 25 err))) (106 (104 25 (105 91 92)) (117 (116 25 93) (123 25
    err)))) (91 (36 (33 (32 err 95) (35 err 66)) (58 (48 err 25) (65 err
    25))) (116 (96 (95 err 25) (97 err 25)) (121 (117 94 25) (122 96 (123
    25 err))))) (97 (65 (48 err (58 25 err)) (95 (91 25 err) (96 25 err)))
    (111 (109 (108 25 97) (110 25 99)) (121 (120 25 98) (123 25 err)))) (47
    (= 42 100 err) (61 (48 101 err) (62 10 err))) (= 12 45 err) (= 9 46
    err) (35 (= 32 47 err) (116 (36 66 err) (117 95 err))) err (11 (10 err
    48) (= 13 102 err)) (13 (= 10 48 err) (32 (14 103 err) (33 104 err)))
    err (95 (58 (48 err 25) (65 err (91 25 err))) (121 (= 96 err 25) (122
    96 (123 25 err)))) (= 32 53 err) (= 61 10 err) (= 61 10 err) (45 (= 43
    106 err) (48 (46 106 err) (58 105 err))) (69 (48 err (58 57 err)) (101
    (70 56 err) (102 56 err))) (58 (47 (46 err 57) (48 err (56 58 21))) (70
    (69 err 56) (= 101 56 err))) (65 (48 err (58 107 err)) (97 (71 107 err)
    (103 107 err))) (= 39 108 err) (99 (48 (11 (10 110 err) (= 39 109 110))
    (58 (49 119 118) (97 110 (98 117 116)))) (114 (103 (102 110 115) (= 110
    114 110)) (117 (115 113 (116 110 112)) (= 118 111 110)))) (= 110 120
    err) (= 114 121 err) (110 (= 108 124 err) (114 (111 123 err) (115 122
    err))) (109 (= 102 125 err) (110 126 (111 127 err))) (105 (100 (= 32 66
    err) (101 67 (102 64 err))) (113 (106 65 (112 err 63)) (= 117 62 err)))
    (= 101 128 err) (= 46 129 err) (= 101 130 err) (= 101 131 err) (115
    (114 err 133) (= 117 132 err)) (= 108 134 err) (= 110 135 err) (110
    (109 err 136) (111 137 err)) (95 (58 (48 err 25) (65 err (91 25 err)))
    (109 (= 96 err 25) (110 138 (123 25 err)))) (96 (65 (48 err (58 25
    err)) (91 25 (95 err 25))) (106 (97 err (105 25 139)) (112 (111 25 140)
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (98 (= 96 err
    25) (99 141 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err)))
    (114 (= 96 err 25) (115 142 (123 25 err)))) (96 (65 (48 err (58 25
    err)) (91 25 (95 err 25))) (106 (97 err (105 25 144)) (109 (108 25 143)
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (97 (96 25
    err) (98 145 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err)))
    (110 (= 96 err 25) (111 146 (123 25 err)))) (95 (58 (48 err 25) (65 err
    (91 25 err))) (97 (96 25 err) (98 147 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (110 (= 96 err 25) (111 148 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (117 (= 96 err 25) (118 149
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (111 (= 96
    err 25) (112 150 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (105 (= 96 err 25) (106 151 (123 25 err)))) (96 (65 (48 err (58
    25 err)) (91 25 (95 err 25))) (106 (97 err (105 25 152)) (116 (115 25
    153) (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25)
    (97 err (123 25 err)))) (96 (65 (48 err (58 25 err)) (91 25 (95 err
    25))) (109 (97 err (108 25 154)) (117 (116 25 155) (123 25 err)))) (95
    (58 (48 err 25) (65 err (91 25 err))) (111 (= 96 err 25) (112 156 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (111 (= 96 err 25)
    (112 157 (123 25 err)))) (96 (65 (48 err (58 25 err)) (91 25 (95 err
    25))) (104 (97 err (103 25 158)) (122 25 (123 159 err)))) (96 (65 (48
    err (58 25 err)) (91 25 (95 err 25))) (114 (97 err (98 161 25)) (115
    160 (123 25 err)))) (65 (35 (= 32 95 err) (48 (36 66 err) (58 25 err)))
    (97 (95 (91 25 err) (96 25 err)) (117 (116 25 94) (123 25 err)))) (35
    (= 32 95 err) (116 (36 66 err) (117 95 err))) (95 (58 (48 err 25) (65
    err (91 25 err))) (112 (= 96 err 25) (113 162 (123 25 err)))) (95 (58
    (48 err 25) (65 err (91 25 err))) (115 (= 96 err 25) (116 163 (123 25
    err)))) (96 (65 (48 err (58 25 err)) (91 25 (95 err 25))) (113 (97 err
    (112 25 164)) (117 (116 25 165) (123 25 err)))) (95 (58 (48 err 25) (65
    err (91 25 err))) (117 (= 96 err 25) (118 166 (123 25 err)))) err err
    (= 13 102 err) (= 10 48 err) err (48 err (58 105 err)) (48 err (58 105
    err)) (65 (48 err (58 107 err)) (97 (71 107 err) (103 107 err))) err (=
    39 167 err) (= 39 167 err) (= 39 168 err) (= 39 169 err) (= 39 170 err)
    (= 39 171 err) (= 39 172 err) (= 39 173 err) (= 39 174 err) (40 (39 err
    167) (48 err (58 175 err))) (40 (39 err 176) (48 err (58 175 err))) (=
    100 177 err) (= 97 178 err) (= 114 179 err) (= 100 180 err) (106 (105
    err 182) (= 115 181 err)) (101 (100 err 184) (= 110 183 err)) (= 112
    185 err) (= 99 186 err) (= 102 187 err) err (= 108 188 err) (= 102 189
    err) (= 98 190 err) (106 (105 err 191) (= 111 192 err)) (= 97 193 err)
    (100 (99 err 194) (101 195 err)) (= 112 196 err) (= 116 197 err) (95
    (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25) (102 198 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (118 (= 96 err 25)
    (119 199 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (116
    (= 96 err 25) (117 200 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (108 (= 96 err 25) (109 201 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (116 (= 96 err 25) (117 202 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (97 (96 25 err) (98 203 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (100 (= 96 err 25)
    (101 204 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (115
    (= 96 err 25) (116 205 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (115 (= 96 err 25) (116 206 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (114 (= 96 err 25) (115 207 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (103 (= 96 err 25) (104 208
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (98 (= 96 err
    25) (99 209 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err)))
    (97 (96 25 err) (98 210 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (110 (= 96 err 25) (111 211 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (111 (= 96 err 25) (112 212 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (105 (= 96 err 25) (106 213
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (105 (= 96
    err 25) (106 214 (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96
    (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (108 (= 96 err 25) (109 215 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (114 (= 96 err 25) (115 216 (123 25 err)))) (95
    (58 (48 err 25) (65 err (91 25 err))) (110 (= 96 err 25) (111 217 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25)
    (102 218 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (117
    (= 96 err 25) (118 219 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (116 (= 96 err 25) (117 220 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (101 (= 96 err 25) (102 221 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25) (102 222
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (108 (= 96
    err 25) (109 223 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (101 (= 96 err 25) (102 224 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (109 (= 96 err 25) (110 225 (123 25 err)))) err
    err err err err err err err (48 err (58 226 err)) err (= 101 227 err)
    (= 103 228 err) (= 111 229 err) (= 105 230 err) (= 101 231 err) (= 102
    232 err) (= 100 233 err) (= 101 234 err) (= 111 235 err) (= 108 236
    err) (= 105 237 err) (= 101 238 err) (= 115 239 err) (= 108 240 err) (=
    118 241 err) (= 116 242 err) (= 115 243 err) (= 111 244 err) err (= 108
    245 err) (= 101 246 err) (95 (58 (48 err 25) (65 err (91 25 err))) (115
    (= 96 err 25) (116 247 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (97 (96 25 err) (98 248 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (101 (= 96 err 25) (102 249 (123 25 err)))) (95
    (58 (48 err 25) (65 err (91 25 err))) (105 (= 96 err 25) (106 250 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (117 (= 96 err 25)
    (118 251 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (116
    (= 96 err 25) (117 252 (123 25 err)))) (91 (58 (48 err 25) (65 err 25))
    (96 (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (115 (= 96 err 25) (116 253 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (116 (= 96 err 25) (117 254 (123 25 err))))
    (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25
    err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (108 (= 96 err 25)
    (109 255 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (116
    (= 96 err 25) (117 256 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (103 (= 96 err 25) (104 257 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (110 (= 96 err 25) (111 258 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (103 (= 96 err 25) (104 259
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (110 (= 96
    err 25) (111 260 (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96
    (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (116 (= 96 err 25) (117 261 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (101 (= 96 err 25) (102 262 (123 25 err)))) (91
    (58 (48 err 25) (65 err 25)) (96 (95 err 263) (97 err (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (99 (= 96 err 25) (100 264
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (105 (= 96
    err 25) (106 265 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (100 (= 96 err 25) (101 266 (123 25 err)))) (91 (58 (48 err 25)
    (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (105 (= 96 err 25) (106 267 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (114 (= 96 err 25) (115 268
    (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97
    err (123 25 err)))) (= 39 269 err) (= 102 270 err) (= 109 271 err) (=
    114 272 err) (= 102 273 err) err err (= 101 274 err) (= 102 275 err) (=
    114 276 err) (= 117 277 err) (= 110 278 err) (= 99 279 err) err (= 105
    280 err) (= 97 281 err) (102 (101 err 282) (= 111 283 err)) (= 115 284
    err) (= 100 285 err) (= 101 286 err) (= 114 287 err) (95 (58 (48 err
    25) (65 err (91 25 err))) (112 (= 96 err 25) (113 288 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (116 (= 96 err 25) (117 289
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (99 (= 96 err
    25) (100 290 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err)))
    (99 (= 96 err 25) (100 291 (123 25 err)))) (95 (58 (48 err 25) (65 err
    (91 25 err))) (97 (96 25 err) (98 292 (123 25 err)))) (95 (58 (48 err
    25) (65 err (91 25 err))) (105 (= 96 err 25) (106 293 (123 25 err))))
    (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25
    err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25)
    (102 294 (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err
    25) (97 err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95
    err 25) (97 err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96
    (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (110 (= 96 err 25) (111 295 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (101 (= 96 err 25) (102 296 (123 25 err)))) (91
    (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (100 (= 96 err 25) (101 297
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (116 (= 96
    err 25) (117 298 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (116 (= 96 err 25) (117 299 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (99 (= 96 err 25) (100 300 (123 25 err)))) (95
    (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25) (102 301 (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (99 (= 96 err 25)
    (100 302 (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (110
    (= 96 err 25) (111 303 (123 25 err)))) err err (= 97 304 err) err err
    (= 102 305 err) err (= 116 306 err) (= 100 307 err) (= 101 308 err) (=
    116 309 err) (= 99 310 err) (= 116 311 err) (= 99 312 err) (= 99 313
    err) err (= 101 314 err) (= 109 315 err) (= 102 316 err) (95 (58 (48
    err 25) (65 err (91 25 err))) (97 (96 25 err) (98 317 (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25) (102 318
    (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (116 (= 96
    err 25) (117 319 (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96
    (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (108 (= 96 err 25) (109 320 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (108 (= 96 err 25) (109 321 (123 25 err)))) (91
    (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err))))
    (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25) (102 322
    (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97
    err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25)
    (97 err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err
    25) (97 err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95
    err 25) (97 err (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96
    (95 err 25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25
    err))) (102 (= 96 err 25) (103 323 (123 25 err)))) (95 (58 (48 err 25)
    (65 err (91 25 err))) (105 (= 96 err 25) (106 324 (123 25 err)))) (91
    (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err))))
    err err err (= 101 325 err) err (= 111 326 err) err (= 101 327 err) (=
    116 328 err) (= 111 329 err) err (= 101 330 err) (= 97 331 err) (95 (58
    (48 err 25) (65 err (91 25 err))) (99 (= 96 err 25) (100 332 (123 25
    err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123
    25 err)))) (95 (58 (48 err 25) (65 err (91 25 err))) (101 (= 96 err 25)
    (102 333 (123 25 err)))) (91 (58 (48 err 25) (65 err 25)) (96 (95 err
    25) (97 err (123 25 err)))) (95 (58 (48 err 25) (65 err (91 25 err)))
    (101 (= 96 err 25) (102 334 (123 25 err)))) (95 (58 (48 err 25) (65 err
    (91 25 err))) (100 (= 96 err 25) (101 335 (123 25 err)))) (91 (58 (48
    err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) (95 (58
    (48 err 25) (65 err (91 25 err))) (116 (= 96 err 25) (117 336 (123 25
    err)))) err (= 114 337 err) err (= 101 338 err) (= 108 339 err) (= 110
    340 err) (= 99 341 err) (95 (58 (48 err 25) (65 err (91 25 err))) (101
    (= 96 err 25) (102 342 (123 25 err)))) (95 (58 (48 err 25) (65 err (91
    25 err))) (100 (= 96 err 25) (101 343 (123 25 err)))) (91 (58 (48 err
    25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) (91 (58 (48
    err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) (91 (58
    (48 err 25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) err (=
    100 344 err) err (= 116 345 err) (= 101 346 err) (91 (58 (48 err 25)
    (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) (91 (58 (48 err
    25) (65 err 25)) (96 (95 err 25) (97 err (123 25 err)))) err (= 97 347
    err) err (= 116 348 err) (= 105 349 err) (= 111 350 err) (= 110 351
    err) err)
   '#((#f . #f) (#f . #f) (89 . 89) (89 . 89) (89 . 89) (89 . 89) (89 . 89)
    (89 . 89) (89 . 89) (89 . 89) (89 . 89) (88 . 88) (87 . 87) (86 . 86)
    (85 . 85) (84 . 84) (83 . 83) (82 . 82) (81 . 81) (80 . 80) (79 . 79)
    (78 . 78) (89 . 89) (89 . 89) (78 . 78) (75 . 75) (#f . #f) (63 . 63)
    (60 . 60) (89 . 89) (#f . #f) (75 . 75) (75 . 75) (75 . 75) (75 . 75)
    (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75)
    (75 . 75) (75 . 75) (89 . 89) (5 . 5) (3 . 3) (2 . 2) (1 . 1) (4 . 4)
    (#f . #f) (60 . 60) (75 . 75) (2 . 2) (89 . 89) (89 . 89) (#f . #f) (78
    . 78) (77 . 77) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (61 . 61) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (#f . #f) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (7 . 7) (6 . 6) (4 . 4) (#f . #f) (0 . 0) (78 . 78) (#f . #f) (76 .
    76) (74 . 74) (74 . 74) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (54 . 54) (#f . #f) (#f . #f) (#f .
    #f) (47 . 47) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (14 . 14) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (73 . 73) (72 . 72) (71 . 71) (70 .
    70) (69 . 69) (68 . 68) (67 . 67) (66 . 66) (#f . #f) (65 . 65) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (38 . 38) (#f . #f) (#f . #f) (75 .
    75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (22 . 22) (75 .
    75) (75 . 75) (21 . 21) (20 . 20) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (12 . 12) (75 . 75) (75 . 75) (75 . 75) (75 .
    75) (75 . 75) (75 . 75) (62 . 62) (75 . 75) (75 . 75) (8 . 8) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (55 . 55) (53 . 53) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (45 . 45) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (75 . 75)
    (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (26 . 26) (25 . 25)
    (75 . 75) (17 . 17) (35 . 35) (24 . 24) (75 . 75) (75 . 75) (19 . 19)
    (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75) (75 . 75)
    (64 . 64) (59 . 59) (#f . #f) (58 . 58) (57 . 57) (#f . #f) (51 . 51)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (39 . 39) (#f . #f) (#f . #f) (#f . #f) (75 . 75) (75 . 75)
    (75 . 75) (27 . 27) (75 . 75) (75 . 75) (18 . 18) (75 . 75) (34 . 34)
    (16 . 16) (13 . 13) (23 . 23) (11 . 11) (75 . 75) (75 . 75) (10 . 10)
    (56 . 56) (52 . 52) (50 . 50) (#f . #f) (48 . 48) (#f . #f) (41 . 41)
    (#f . #f) (#f . #f) (#f . #f) (44 . 44) (#f . #f) (#f . #f) (75 . 75)
    (29 . 29) (75 . 75) (32 . 32) (75 . 75) (75 . 75) (9 . 9) (75 . 75) (49
    . 49) (#f . #f) (43 . 43) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (75 .
    75) (75 . 75) (30 . 30) (15 . 15) (33 . 33) (46 . 46) (#f . #f) (40 .
    40) (#f . #f) (#f . #f) (31 . 31) (28 . 28) (42 . 42) (#f . #f) (36 .
    36) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (37 . 37))))

;
; User functions
;

(define lexer #f)

(define lexer-get-line   #f)
(define lexer-getc       #f)
(define lexer-ungetc     #f)

(define lexer-init
  (lambda (input-type input)
    (let ((IS (lexer-make-IS input-type input 'line)))
      (set! lexer (lexer-make-lexer lexer-default-table IS))
      (set! lexer-get-line   (lexer-get-func-line IS))
      (set! lexer-getc       (lexer-get-func-getc IS))
      (set! lexer-ungetc     (lexer-get-func-ungetc IS)))))
