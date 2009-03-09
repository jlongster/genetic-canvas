;;;; Implements a genetic algorithm for learning how to draw images.
;;;;
;;;; 3/9/2009
;;;; James Long

;; Declares

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; Required modules

(include "ffi/util.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "ffi/ffi"))
(load (resource lib-path "ffi/freeimage"))
(load (resource lib-path "util/sort.scm"))
(load (resource lib-path "util/srfi-1.scm"))
(load (resource lib-path "util/srfi-13.scm"))

;; Utility

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define source-image #f)
(define texture-buffer #f)

(define population #f)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (midpoint point1 point2 point3)
  (let ((point (vec2-add point1
                          (vec2-scalar-mul
                           (vec2-sub point2 point1) .5))))
    (vec2-add point
               (vec2-scalar-mul
                (vec2-sub point3 point) .5))))

(define (float-vec2->int-vec2 v)
  (make-vec2 (inexact->exact (floor (vec2-x v)))
             (inexact->exact (floor (vec2-y v)))))

(define (ubyte-vec3->float-vec3 v)
  (make-vec3 (exact->inexact (/ (vec3-x v) 255))
             (exact->inexact (/ (vec3-y v) 255))
             (exact->inexact (/ (vec3-z v) 255))))  


;; Vectors

(define-structure vec2
  x y)

(define-structure vec3
  x y z)

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
  (make-vec2 (* (random-real) (or scale 1.0))
             (* (random-real) (or scale 1.0))))

(define (random-vec3 #!optional scale)
  (make-vec3 (* (random-real) (or scale 1.0))
             (* (random-real) (or scale 1.0))
             (* (random-real) (or scale 1.0))))


;; Images

(define-type image
  id: EF8C0406-A5C4-48A6-BC1B-B615E1FEEFA6
  constructor: really-make-image
  width
  height
  rgb-bytes
  gl-texture-id)

(define (make-image filename)
  (if (not (or (not (eq? (string-suffix-length filename ".jpg") 0))
               (not (eq? (string-suffix-length filename ".jpeg") 0))))
      (raise '("Image type not supported", filename)))  
  (let ((img (freeimage-load-jpg filename)))
    (really-make-image (freeimage-width img)
                       (freeimage-height img)
                       (freeimage-bytes img)
                       #f)))

(define (image-opengl-upload! image)
  (let ((tex (with-alloc (buf (alloc-uint 1))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGB
                  (image-width image)
                  (image-height image)
                  0
                  GL_RGB
                  GL_UNSIGNED_BYTE
                  (u8*->void* (image-rgb-bytes image)))
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MIN_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MAG_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_S
                     GL_REPEAT)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_T
                     GL_REPEAT)
    (glBindTexture GL_TEXTURE_2D 0)
    (image-gl-texture-id-set! image tex)))

(define (image-fetch-rgb image x y)
  (let ((x (min x (- (image-width image) 1)))
        (y (min y (- (image-height image) 1))))
    (let* ((offset (+ (* y (image-width image)) x))
           (byte-offset (* offset 3))
           (data (image-rgb-bytes image)))
      (make-vec3 (char->integer (u8*-ref data byte-offset))
                 (char->integer (u8*-ref data (+ byte-offset 1)))
                 (char->integer (u8*-ref data (+ byte-offset 2)))))))


;; Genotype

(define-structure triangle
  point1
  point2
  point3
  color)

(define-type genotype
  id: 6446D535-85C7-45DE-B3C7-32A2E2CD0658
  constructor: really-make-genotype
  data
  fitness)

(define (make-genotype data #!optional fitness)
  (really-make-genotype data (or fitness 0)))

(define (random-genotype)
  (make-genotype
   (let loop ((acc '())
              (i 0))
     (if (< i 100)
         (let* ((scale (* (current-width) 1.5))
                (p1 (random-vec2 scale))
                (p2 (random-vec2 scale))
                (p3 (random-vec2 scale))
                (midpoint (float-vec2->int-vec2 (midpoint p1 p2 p3)))
                (tri (make-triangle
                      p1 p2 p3
                      (ubyte-vec3->float-vec3
                       (image-fetch-rgb source-image
                                        (vec2-x midpoint)
                                        (vec2-y midpoint))))))
           (loop (cons tri acc) (+ i 1)))
         acc))
   0.0))


;; Selection procedures

(define (selection-rws pop f)
  (let loop ((lst pop)
             (ptr 0))
    (let ((gt (car lst)))
      (if (or
           (null? (cdr lst))
           (and (>= f ptr)
                (< f (+ ptr (genotype-fitness gt)))))
          gt 
          (loop (cdr lst) (+ ptr (genotype-fitness gt)))))))

(define (selection-sus pop n)
  (let* ((total (fold (lambda (el acc)
                        (+ (genotype-fitness el) acc))
                      0
                      pop))
         (step (inexact->exact (ceiling (/ total n))))
         (start (random-integer step)))
    (let loop ((acc '())
               (i 0))
      (if (< i n)
          (loop (cons (selection-rws pop (+ start (* i step)))
                      acc)
                (+ i 1))
          (reverse acc)))))


;; Crossover procedures

(define default-crossover-rate .01)

(define (genotype-crossover gt1 gt2 #!optional rate-or-pt)

  (define (split gt pt)
    (let loop ((lst (genotype-data gt))
               (acc1 '())
               (acc2 '())
               (i 0))
      (if (null? lst)
          (values (reverse acc1)
                  (reverse acc2))
          (if (< i pt)
              (loop (cdr lst) (cons (car lst) acc1) acc2 (+ i 1))
              (loop (cdr lst) acc1 (cons (car lst) acc2) (+ i 1))))))

  (if (or (fixnum? rate-or-pt)
          (< (random-real)
             (or (and (flonum? rate-or-pt) rate-or-pt)
                 default-crossover-rate)))
      (let* ((len1 (length (genotype-data gt1)))
             (len2 (length (genotype-data gt2)))
             (len (max len1 len2))
             (pt (or (and (fixnum? rate-or-pt) rate-or-pt)
                     (random-integer len))))
        (receive (front1 back1) (split gt1 pt)
          (receive (front2 back2) (split gt2 pt)
            (values (make-genotype (append front1 back2))
                    (make-genotype (append front2 back1))))))
      (values gt1 gt2)))



;; Mutation procedures

(define default-mutation-rate .01)

(define (mutate-point point)
  (vec2-add point (make-vec2 (* (random-real) 5)
                             (* (random-real) 5))))

(define (mutate-triangle triangle)
  (let* ((point1 (mutate-point (triangle-point1 triangle)))
         (point2 (mutate-point (triangle-point2 triangle)))
         (point3 (mutate-point (triangle-point3 triangle)))
         (midpoint (float-vec2->int-vec2 (midpoint point1 point2 point3))))
    (make-triangle point1 point2 point3
                   (ubyte-vec3->float-vec3
                    (image-fetch-rgb source-image
                                     (vec2-x midpoint)
                                     (vec2-y midpoint))))))

(define (genotype-mutate gt #!optional rate)
  (make-genotype
   (let loop ((acc '())
              (data (genotype-data gt)))
     (if (null? data)
         (reverse acc)
         (loop (cons (if (< (random-real)
                            (or rate default-mutation-rate))
                         (mutate-triangle (car data))
                         (car data))
                     acc)
               (cdr data))))))


;; Bootstrapping a population

(define (make-population size)
  (let loop ((acc '())
             (i 0))
    (if (< i size)
        (loop (cons (random-genotype) acc)
              (+ i 1))
        acc)))

(define (population-normalize pop best-fitness worst-fitness)
  (let loop ((pop pop))
    (if (not (null? pop))
        (let ((gt (car pop)))
          (genotype-fitness-set! gt (- worst-fitness
                                       (genotype-fitness gt)))
          (loop (cdr pop))))))

(define (population-evolve pop)
  (let* ((count (length pop))
         (sorted (sort pop
                       (lambda (x y) (< (genotype-fitness x)
                                        (genotype-fitness y)))))
         (pop (selection-sus sorted count)))
    (set! population
          (let loop ((acc '())
                     (tail pop))
            (cond
             ((null? tail) acc)
             ((eq? (cdr tail) '())
              (reverse (cons (genotype-mutate (car tail)) acc)))
             (else
              (let ((gt1 (car tail))
                    (gt2 (cadr tail))
                    (tail (cddr tail)))
                (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2)
                  (loop (cons (genotype-mutate new-gt2)
                              (cons (genotype-mutate new-gt1) acc))
                        tail)))))))))

(define (render-triangle tri)
  (let ((point1 (triangle-point1 tri))
        (point2 (triangle-point2 tri))
        (point3 (triangle-point3 tri))
        (color (triangle-color tri)))
    (glColor4f (vec3-x color) (vec3-y color) (vec3-z color) 0.1)
    (glVertex2f (vec2-x point1) (vec2-y point1))
    (glVertex2f (vec2-x point2) (vec2-y point2))
    (glVertex2f (vec2-x point3) (vec2-y point3))))

(define (run-genotype gt)
  ;; Draw genotype
  (glClear GL_COLOR_BUFFER_BIT)
  (glBegin GL_TRIANGLES)
  (let loop ((tris (genotype-data gt)))
    (if (not (null? tris))
        (begin
          (render-triangle (car tris))
          (loop (cdr tris)))))
  (glEnd)
  (glColor4f 0.0 0.0 0.0 1.0)

  ;; Draw mona lisa    
  (glEnable GL_COLOR_LOGIC_OP)
  (glLogicOp GL_XOR)
  (glBindTexture GL_TEXTURE_2D (image-gl-texture-id source-image))
  (glBegin GL_QUADS)
  (begin
    (glTexCoord2d 1. 1.)
    (glVertex2f 0. 0.))
  (begin
    (glTexCoord2d 1. 0.)
    (glVertex2f 0. (current-height)))
  (begin
    (glTexCoord2d 0. 0.)
    (glVertex2f (current-width)
                (current-height)))
  (begin
    (glTexCoord2d 0. 1.)
    (glVertex2f (current-width) 0.))
  (glEnd)
  (glBindTexture GL_TEXTURE_2D 0)
  (glDisable GL_COLOR_LOGIC_OP)

  (let* ((widthi (inexact->exact (current-width)))
         (heighti (inexact->exact (current-height))))
    (glReadPixels 0 0
                  widthi heighti
                  GL_RGB GL_UNSIGNED_BYTE
                  (u8*->void* texture-buffer))
    (sum-u8* texture-buffer
             (* widthi heighti 3))))


;; Application
;; These are special methods which the application is aware of,
;; and are automatically called at the appropriate times.

(define (init-engine width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))

  (set! source-image (make-image (resource "resources/monalisa.jpg")))
  (set! population (make-population 100))
  (freeimage-initialize #f))

(define (shutdown-engine)
  (freeimage-deinitialize))

(define (init-opengl)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.
              (current-width)
              (current-height)
              0.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glEnable GL_TEXTURE_2D)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

  (image-opengl-upload! source-image)
  
  (set! texture-buffer (alloc-u8 (inexact->exact (* (current-width)
                                                    (current-height)
                                                    4)))))

(define (run-frame)
  (glClearColor 1. 1. 1. 1.)
  (glLoadIdentity)

  ;; Run all the genotypes
  (let loop ((pop population)
             (best 0)
             (worst 0))
    (if (null? pop)
        (population-normalize population best worst)
        (let* ((gt (car pop))
               (fitness (run-genotype gt)))
          (genotype-fitness-set! gt fitness)
          (loop (cdr pop)
                (min best fitness)
                (max worst fitness)))))

  (glClear GL_COLOR_BUFFER_BIT)
  (glBegin GL_TRIANGLES)
  (let loop ((tris (genotype-data (car population))))
    (if (not (null? tris))
        (begin
          (render-triangle (car tris))
          (loop (cdr tris)))))
  (glEnd)
  (glColor4f 0.0 0.0 0.0 1.0)
  
  (population-evolve population))
