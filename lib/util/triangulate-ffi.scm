
(c-declare "#include \"triangulate.c\"")

(c-define-type c-vec2 (struct "vec2"))
(c-define-type c-vec2* (pointer c-vec2))

(define make-c-vec2
  (c-lambda (float float) c-vec2* #<<end-c-code
   vec2 *v = (vec2*)malloc(sizeof(vec2));
   (*v).x = ___arg1;
   (*v).y = ___arg2;
   ___result_voidstar = v;
end-c-code
))

(define c-vec2-x
  (c-lambda (c-vec2*) float "___result = (*___arg1).x;"))

(define c-vec2-y
  (c-lambda (c-vec2*) float "___result = (*___arg1).y;"))

(define c-vec2-x-set!
  (c-lambda (c-vec2* float) void
            "(*___arg1).x = ___arg2;"))

(define c-vec2-y-set!
  (c-lambda (c-vec2* float) void
            "(*___arg1).y = ___arg2;"))

(define alloc-c-vec2
  (c-lambda (int) c-vec2*
            "___result_voidstar = malloc(___arg1*sizeof(vec2));"))

(define c-vec2*-ref
  (c-lambda (c-vec2* int) c-vec2*
            "___result_voidstar = (___arg1+(___arg2*sizeof(vec2)));"))

(define (vec2-list->c-vec2-vector data)
  (let* ((count (length data))
         (vec (alloc-c-vec2 count)))
    (let loop ((i 0)
               (tail data))
      (if (null? tail)
          vec
          (let ((head (car tail))
                (inst (c-vec2*-ref vec i)))
            (c-vec2-x-set! inst (vec2-x head))
            (c-vec2-y-set! inst (vec2-y head))
            (loop (+ i 1) (cdr tail)))))))

(define triangulate
  (c-lambda (c-vec2* int) c-vec2* "triangulate"))
