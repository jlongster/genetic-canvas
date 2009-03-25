;; We can't use SchemeWEB because it messes up our C code. This is
;; here just for historical records, really, as the program doesn't
;; use this triangulation library anymore. This'll probably go away at
;; some point.

(load "../lib/util/srfi-1")
(include "../lib/util/tests.scm")
(load "../lib/vectors")
(load "../lib/ffi/triangulate-ffi")
(load "../lib/ffi/ffi")
(include "../lib/ffi/triangulate-ffi#.scm")
(c-declare "#include \"../lib/ffi/triangulate.h\"")

(define get-c-vec2*
  (c-lambda () c-vec2* #<<end-c-code
vec2 *v = calloc(3, sizeof(vec2));

v[0].x = 5.0;
v[0].y = 10.0;

v[1].x = -1.0;
v[1].y = 3.0;

v[2].x = 2.0;
v[2].y = 4.0;

___result_voidstar = v;
end-c-code
))

(define-test c-vec2
  (let ((lst (get-c-vec2*)))
    (let ((one (c-vec2*-ref lst 0))
          (two (c-vec2*-ref lst 1))
          (three (c-vec2*-ref lst 2)))
      (assert-equal (c-vec2-x one) 5.)
      (assert-equal (c-vec2-y one) 10.)
      (assert-equal (c-vec2-x two) -1.)
      (assert-equal (c-vec2-y two) 3.)
      (assert-equal (c-vec2-x three) 2.)
      (assert-equal (c-vec2-y three) 4.))))

(define get-c-vec2-vector
  (c-lambda () c-vec2-vector #<<end-c-code
vec2_vector vec;
vec2 *v = calloc(3, sizeof(vec2));

v[0].x = 5.0;
v[0].y = 10.0;

v[1].x = -1.0;
v[1].y = 3.0;

v[2].x = 2.0;
v[2].y = 4.0;

vec.data = v;
vec.length = 3;

___result = vec;
end-c-code
))

(define-test c-vec2-vector->vec2-list
  (let* ((lst (c-vec2-vector->vec2-list (get-c-vec2-vector)))
         (first (car lst))
         (second (cadr lst))
         (third (caddr lst)))
    (assert-equal (length lst) 3)
    (assert-equal (vec2-x first) 5.)
    (assert-equal (vec2-y first) 10.)
    (assert-equal (vec2-x second) -1.)
    (assert-equal (vec2-y second) 3.)
    (assert-equal (vec2-x third) 2.)
    (assert-equal (vec2-y third) 4.)))

(define (print-c-vec2 vec)
  (display (c-vec2-x vec))
  (display " ")
  (display (c-vec2-y vec))
  (display "\n"))

(define-test vec2-list->c-vec2-vector
  (let* ((lst (list (make-vec2 5. 10.)
                    (make-vec2 16. -6.)
                    (make-vec2 10. 14.)
                    (make-vec2 12. 15.)))
         (c-vec (vec2-list->c-vec2-vector lst))
         (last (c-vec2*-ref c-vec 3)))
    (assert-equal (c-vec2-x last) 12.)
    (assert-equal (c-vec2-y last) 15.)))

(define-test triangulate
  (define points (list (make-vec2 15. 10.)
                       (make-vec2 16. 26.)
                       (make-vec2 10. 14.)
                       (make-vec2 12. -5.)))
  (pp (triangulate points)))
