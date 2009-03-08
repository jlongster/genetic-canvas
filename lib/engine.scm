
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "ffi/util.scm")

(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "ffi/ffi"))
(load (resource lib-path "ffi/freeimage"))
(load (resource lib-path "util/sort.scm"))
(load (resource lib-path "util/srfi-1.scm"))

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define mona-lisa-tex #f)
(define mona-lisa-tex2 #f)
(define texture-buffer #f)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (init-engine)
  (freeimage-initialize #f))

(define (shutdown-engine)
  (freeimage-deinitialize))

(define (load-jpg filename)
  (let ((img (freeimage-load-jpg filename))
        (tex (with-alloc (buf (alloc-uint 1))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (with-alloc (bytes (freeimage-bytes img))
                (glTexImage2D GL_TEXTURE_2D
                              0
                              GL_RGB
                              (freeimage-width img)
                              (freeimage-height img)
                              0
                              GL_RGB
                              GL_UNSIGNED_BYTE
                              (u8*->void* (freeimage-bytes img))))
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
    tex))

(define (init-opengl width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))

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

  (set! mona-lisa-tex (load-jpg (resource "resources/monalisa.jpg")))
  (set! mona-lisa-tex2 (load-jpg (resource "resources/monalisa2.jpg")))
  (set! texture-buffer
        (alloc-u8
         (inexact->exact (* (current-width) (current-height) 4)))))


;; Geometric information

(define-structure vec2d
  x y)

(define-structure vec3d
  x y z)

(define (random-vec2d)
  (make-vec2d (random-real) (random-real)))

(define (random-vec3d)
  (make-vec3d (random-real) (random-real) (random-real)))

(define-structure triangle
  point1
  point2
  point3
  color)


;; Genotype

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
         (loop (cons (make-triangle (random-vec2d)
                                    (random-vec2d)
                                    (random-vec2d)
                                    (random-vec3d))
                     acc)
               (+ i 1))
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

(define crossover-rate .01)

(define (genotype-crossover gt1 gt2 #!optional pt)

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
  
  (let* ((len1 (length (genotype-data gt1)))
         (len2 (length (genotype-data gt2)))
         (len (max len1 len2))
         (pt (or pt (random-integer len))))
    (receive (front1 back1) (split gt1 pt)
      (receive (front2 back2) (split gt2 pt)
        (values (make-genotype (append front1 back2))
                (make-genotype (append front2 back1)))))))



;; Mutation procedures

(define mutation-rate .01)

(define (genotype-mutate gt)
  )

(define (make-population size)
  (let loop ((acc '())
             (i 0))
    (if (< i size)
        (loop (cons (random-genotype) acc)
              (+ i 1))
        acc)))

(define (population-normalize pop worst-fitness)
  (let loop ((pop pop))
    (if (not (null? pop))
        (let ((gt (car pop)))
          (genotype-fitness-set! (- (genotype-fitness gt)
                                    worst-fitness))
          (loop (cdr pop))))))

(define (population-evolve pop)  
  (let* ((pop (sort pop
                    (lambda (x y) (> (genotype-fitness x)
                                     (genotype-fitness y)))))
         (count (length pop)))
    (let loop ((acc '())
               (i 0))
      (if (< i count)
          (let ((one (select-genome pop))
                (two (select-genome pop)))
            (genome-crossover! one two)
            (loop (cons (genome-mutate! one)
                        (cons (genome-mutate! two) acc))))
          acc))))

(define (population-crossover size)
  (void))

(define population (make-population 100))

(define (render-triangle tri)
  (let ((point1 (triangle-point1 tri))
        (point2 (triangle-point2 tri))
        (point3 (triangle-point3 tri))
        (color (triangle-color tri)))
    (begin (glColor4f (* (vec3d-x color) 1.0)
                      (* (vec3d-y color) 1.0)
                      (* (vec3d-z color) 1.0)
                      0.1)
           (glVertex2f (* (vec2d-x point1) (current-width))
                       (* (vec2d-y point1) (current-height)))
           (glVertex2f (* (vec2d-x point2) (current-width))
                       (* (vec2d-y point2) (current-height)))
           (glVertex2f (* (vec2d-x point3) (current-width))
                       (* (vec2d-y point3) (current-height))))))

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
  (glBindTexture GL_TEXTURE_2D mona-lisa-tex)
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

(define (run-frame)
  (glClearColor 1. 1. 1. 1.)
  (glLoadIdentity)

  ;; Run all the genotypes
  (let loop ((pop population)
             (worst-fitness 0))
    (if (null? pop)
        (population-normalize population worst-fitness)
        (let* ((gt (car pop))
               (fitness (run-genotype )))
          (genotype-fitness-set! gt fitness)
          (loop (cdr pop) (max worst-fitness fitness)))))

  (population-evolve population))
