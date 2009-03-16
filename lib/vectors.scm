
;; Vectors

(define-structure vec2
  x y)

(define-structure vec3
  x y z)

(define (vec2-equal? v1 v2)
  (and (eq? (vec2-x v1) (vec2-x v2))
       (eq? (vec2-y v1) (vec2-y v2))))

(define (vec2-op v1 v2 op)
  (make-vec2 (op (vec2-x v1) (vec2-x v2))
             (op (vec2-y v1) (vec2-y v2))))

(define (vec2-add v1 v2)
  (vec2-op v1 v2 +))

(define (vec2-sub v1 v2)
  (vec2-op v1 v2 -))

(define (vec2-component-mul v1 v2)
  (vec2-op v1 v2 *))

(define (vec2-scalar-mul v1 f)
  (make-vec2 (* (vec2-x v1) f)
             (* (vec2-y v1) f)))

(define (vec2-length v1)
  (sqrt (+ (* (vec2-x v1) (vec2-x v1))
           (* (vec2-y v1) (vec2-y v1)))))

(define (vec3-equal? v1 v2)
  (and (eq? (vec3-x v1) (vec3-x v2))
       (eq? (vec3-y v1) (vec3-y v2))
       (eq? (vec3-z v1) (vec3-z v2))))

(define (vec3-op v1 v2 op)
  (make-vec3 (op (vec3-x v1) (vec3-x v2))
              (op (vec3-y v1) (vec3-y v2))
              (op (vec3-z v1) (vec3-z v2))))

(define (vec3-add v1 v2)
  (vec3-op v1 v2 +))

(define (vec3-sub v1 v2)
  (vec3-op v1 v2 -))

(define (vec3-component-mul v1 v2)
  (vec3-op v1 v2 *))

(define (vec3-scalar-mul v1 f)
  (make-vec3 (* (vec3-x v1) f)
             (* (vec3-y v1) f)
             (* (vec3-z v1) f)))

(define (vec3-length v1)
  (flsqrt (vec3-dot v1 v1)))

(define (vec3-unit v1)
  (vec3-scalar-mul v1 (/ (vec3-length v1))))

(define (vec3-dot v1 v2)
  (+ (* (vec3-x v1) (vec3-x v2))
     (* (vec3-y v1) (vec3-y v2))
     (* (vec3-z v1) (vec3-z v2))))

(define (vec3-cross v1 v2)
  (let ((v1-x (vec3-x v1)) (v2-x (vec3-x v2))
	(v1-y (vec3-y v1)) (v2-y (vec3-y v2))
	(v1-z (vec3-z v1)) (v2-z (vec3-z v2)))
    (make-vec3 (- (* v1-y v2-z)
                  (* v1-z v2-y))
               (- (* v1-z v2-x)
                  (* v1-x v2-z))
               (- (* v1-x v2-y)
                  (* v1-y v2-x)))))

(define (random-vec2 #!optional scale)
  (make-vec2 (* (- (* (random-real) 2.) 1.) (or scale 1.0))
             (* (- (* (random-real) 2.) 1.) (or scale 1.0))))

(define (random-vec3 #!optional scale)
  (make-vec3 (* (random-real) (or scale 1.0))
             (* (random-real) (or scale 1.0))
             (* (random-real) (or scale 1.0))))
