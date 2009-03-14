
(load (resource lib-path "genetic-operators"))

;; Genotype

(define-structure polygon
  points
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
     (if (< i 150)
         (let* ((random-point (lambda ()
                               (make-vec2
                                (* (random-real)
                                   (exact->inexact (current-width)))
                                (* (random-real)
                                   (exact->inexact (current-height))))))
                (p (random-point))
                (random-color (image-fetch-rgb source-image
                                               (inexact->exact (floor (vec2-x p)))
                                               (inexact->exact (floor (vec2-y p))))))
           (loop (cons (make-polygon
                        (unfold (lambda (i) (>= i 6))
                                (lambda (i) (random-point))
                                (lambda (i) (+ i 1))
                                0)
                        0. 0. 0. 0.001)
                       acc)
                 (+ i 1)))
         acc))
   0.0))

(define (%%render-polygon tri)
  (glBegin GL_POLYGON)
  (glColor4f (polygon-red tri)
             (polygon-green tri)
             (polygon-blue tri)
             (polygon-alpha tri))
  (let loop ((tail (polygon-points tri)))
    (if (not (null? tail))
        (let ((point (car tail)))
          (glVertex2f (vec2-x point) (vec2-y point))
          (loop (cdr tail)))))
  (glEnd))

(define (render-genotype gt)
  (let loop ((tris (genotype-data gt)))
    (if (not (null? tris))
        (begin
          (%%render-polygon (car tris))
          (loop (cdr tris))))))

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


;; Population

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
                       (lambda (x y) (> (genotype-fitness x)
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
              (reverse (cons (genotype-mutate-light (car tail)) acc)))
             (else
              (let ((gt1 (car tail))
                    (gt2 (cadr tail))
                    (tail (cddr tail)))
                (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2)
                  (loop (cons (genotype-mutate-light new-gt2)
                              (cons (genotype-mutate-light new-gt1) acc))
                        tail)))))))))


;; Selection procedures

(define (selection-rws pop f #!optional (get-fitness genotype-fitness))
  (let loop ((lst pop)
             (ptr 0))
    (let ((gt (car lst)))
      (if (or
           (null? (cdr lst))
           (and (>= f ptr)
                (< f (+ ptr (get-fitness gt)))))
          gt 
          (loop (cdr lst) (+ ptr (get-fitness gt)))))))

(define (selection-sus pop n #!optional (get-fitness genotype-fitness))
  (let* ((total (fold (lambda (el acc)
                        (+ (get-fitness el) acc))
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

(define default-crossover-rate 0)

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

(define default-mutation-rate .5)

(define (half-negate f)
  (- (* f 2.) 1.))

(define (saturate f)
  (if (< f 0.) 0.
      (if (> f 1.) 1.
          f)))

(define (mutate-point point)
  (make-vec2 (+ (vec2-x point) (* (half-negate (random-real)) 50))
             (+ (vec2-y point) (* (half-negate (random-real)) 50))))

(define (mutate-real real)
  (saturate (+ (* (half-negate (random-real)))
               real)))

(define-type mutator
  func
  weight)

(define mutators
  (list (make-mutator
         (lambda (poly)
           (let* ((points (list->vector (polygon-points poly)))
                  (idx (random-integer (vector-length points))))
             (vector-set! points idx
                          (mutate-point (vector-ref points idx)))
             (polygon-points-set! poly (vector->list points))))
         4)
        (make-mutator
         (lambda (poly)
           (polygon-red-set! poly (random-real)))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-green-set! poly (random-real)))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-blue-set! poly (random-real)))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-alpha-set! poly (* (random-real) .5)))
         1)))

(define mutator-weight-range (fold (lambda (el acc)
                                     (+ acc (mutator-weight el)))
                                   0
                                   mutators))

(define (mutate-polygon! polygon)
  (let ((mutator (selection-rws mutators
                                (random-integer mutator-weight-range)
                                mutator-weight)))
    ((mutator-func mutator) polygon)))

(define (genotype-mutate-heavy gt #!optional rate)
  (make-genotype
   (let loop ((acc '())
              (data (genotype-data gt)))
     (if (null? data)
         (reverse acc)
         (loop (cons (if (< (random-real)
                            (or rate default-mutation-rate))
                         (let* ((poly (car data))
                                (new-poly (make-polygon (polygon-points poly)
                                                        (polygon-red poly)
                                                        (polygon-green poly)
                                                        (polygon-blue poly)
                                                        (polygon-alpha poly))))
                           (mutate-polygon! new-poly)
                           new-poly)
                         (car data))
                     acc)
               (cdr data))))))

(define (genotype-mutate-light gt #!optional rate)
  (make-genotype
   (let* ((polys (list->vector (genotype-data gt)))
          (idx (random-integer (vector-length polys)))
          (poly (vector-ref polys idx))
          (new-poly (make-polygon (polygon-points poly)
                                  (polygon-red poly)
                                  (polygon-green poly)
                                  (polygon-blue poly)
                                  (polygon-alpha poly))))
     (mutate-polygon! new-poly)
     (vector-set! polys idx new-poly)
     (vector->list polys))))
