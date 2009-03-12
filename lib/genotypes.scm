
(load (resource lib-path "genetic-operators"))

;; Genotype

(define-structure triangle
  point1
  point2
  point3
  red
  green
  blue
  alpha)

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
     (if (< i 50)
         (let ((random-point (lambda ()
                               (make-vec2
                                (* (random-real)
                                   (exact->inexact (current-width)))
                                (* (random-real)
                                   (exact->inexact (current-height))))))
               (random-count (+ (random-integer 4) 3)))
           (loop (cons (make-triangle
                        (random-point)
                        (random-point)
                        (random-point)
                        (random-real)
                        (random-real)
                        (random-real)
                        (random-real))
                       acc)
                 (+ i 1)))
         acc))
   0.0))

(define (%%render-triangle tri)
  (let ((point1 (triangle-point1 tri))
        (point2 (triangle-point2 tri))
        (point3 (triangle-point3 tri)))
    (glColor4f (triangle-red tri)
               (triangle-green tri)
               (triangle-blue tri)
               (triangle-alpha tri))
    (glVertex2f (vec2-x point1) (vec2-y point1))
    (glVertex2f (vec2-x point2) (vec2-y point2))
    (glVertex2f (vec2-x point3) (vec2-y point3))))

(define (render-genotype gt)
  (glBegin GL_TRIANGLES)
  (let loop ((tris (genotype-data gt)))
    (if (not (null? tris))
        (begin
          (%%render-triangle (car tris))
          (loop (cdr tris)))))
  (glEnd))

(define (run-genotype gt)
  ;; Draw genotype
  (glClear GL_COLOR_BUFFER_BIT)
  (render-genotype gt)
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


;; Managing a population

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

(define (population-evolve! pop)
  (let* ((count (length pop))
         (sorted (sort pop
                       (lambda (x y) (< (genotype-fitness x)
                                        (genotype-fitness y)))))
         (pop (unfold (lambda (i) (>= i count))
                      (lambda (i) (make-genotype
                                   (genotype-data (car sorted))))
                      (lambda (i) (+ i 1))
                      0)
          ;; (selection-sus sorted count)
              ))
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

