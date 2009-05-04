
(declare (block)
         (standard-bindings)
         (extended-bindings)
         (fixnum))

;; Genotype

(define-type genotype
  id: 6446D535-85C7-45DE-B3C7-32A2E2CD0658
  constructor: really-make-genotype
  data
  fitness)

(define (make-genotype data #!optional fitness)
  (really-make-genotype data (or fitness 0.)))

(define (genotype-shallow-copy gt)
  (make-genotype (genotype-data gt) 0.))

(define (genotype-equal? gt1 gt2)
  (let ((data1 (genotype-data gt1))
        (data2 (genotype-data gt2)))
    (let loop ((tail1 data1)
               (tail2 data2)
               (same #t))
      (if (or (not same)
              (null? tail1)
              (null? tail2))
          same
          (loop (cdr tail1)
                (cdr tail2)
                (polygon-equal? (car tail1)
                                (car tail2)))))))

(define (random-genotype)
  (declare (fixnum))
  (make-genotype
   (let loop ((acc '())
              (i 0))
     (if (< i initial-num-polygons)
         (loop (cons (random-polygon) acc)
               (+ i 1))
         acc))
   0.))

(define (render-genotype gt)
  (let loop ((tris (genotype-data gt)))
    (if (not (null? tris))
        (begin
          (render-polygon (car tris))
          (loop (cdr tris))))))

(define %%genotype-current-image #f)

(define (run-genotype gt source-image)
  ;; Allocate an image buffer if necessary
  (if (not %%genotype-current-image)
      (set! %%genotype-current-image
            (make-image (image-width source-image)
                        (image-height source-image)
                        FORMAT_RGBA)))

  ((if hardware-accelerated?
       calculate-fitness/accelerated
       calculate-fitness)
   source-image gt))

(define %%genotype-framebuffer #f)
(define %%genotype-framebuffer-img0 #f)
(define %%genotype-framebuffer-img1 #f)
(define %%genotype-framebuffer-img2 #f)

;; This does the same thing as calculate-fitness, but applies the
;; difference of the two images on the graphics card. It turns out
;; that because of fill rates and possibly other things, this is
;; slower than calculating it all on the CPU. :(
(define (calculate-fitness/accelerated source-image gt)
  (if (not %%genotype-framebuffer)
      (begin
        (let ((fbo (alloc-framebuffer))
              (img0 (alloc-framebuffer-image (image-width source-image)
                                             (image-height source-image)))
              (img1 (alloc-framebuffer-image (image-width source-image)
                                             (image-height source-image)))
              (img2 (alloc-framebuffer-image (image-width source-image)
                                             (image-height source-image))))
          (set! %%genotype-framebuffer fbo)
          (set! %%genotype-framebuffer-img0 img0)
          (set! %%genotype-framebuffer-img1 img1)
          (set! %%genotype-framebuffer-img2 img2)

          (framebuffer-select fbo)
          (framebuffer-attach-image GL_COLOR_ATTACHMENT0_EXT (image-gl-texture-id img0))
          (framebuffer-attach-image GL_COLOR_ATTACHMENT1_EXT (image-gl-texture-id img1))
          (framebuffer-attach-image GL_COLOR_ATTACHMENT2_EXT (image-gl-texture-id img2))
          (framebuffer-check-status)
          (framebuffer-select 0))))
  
  (framebuffer-select %%genotype-framebuffer)
  (glPushAttrib GL_COLOR_BUFFER_BIT)

  (glDrawBuffer GL_COLOR_ATTACHMENT0_EXT)
  (glClear GL_COLOR_BUFFER_BIT)
  (render-genotype gt)
  
  (glDrawBuffer GL_COLOR_ATTACHMENT1_EXT)
  (image-render source-image)
  (glBlendEquation GL_MAX)
  (glBlendFunc GL_ONE GL_ONE)
  (image-render %%genotype-framebuffer-img0)

  (glDrawBuffer GL_COLOR_ATTACHMENT2_EXT)
  (glBlendEquation GL_FUNC_ADD)
  (glBlendFunc GL_ONE GL_ZERO)
  (image-render source-image)
  (glBlendEquation GL_MIN)
  (glBlendFunc GL_ONE GL_ONE)
  (image-render %%genotype-framebuffer-img0)

  (framebuffer-select 0)
  
  (glBlendEquation GL_FUNC_ADD)
  (glBlendFunc GL_ONE GL_ZERO)
  (image-render %%genotype-framebuffer-img2)
  (glBlendEquation GL_FUNC_SUBTRACT)
  (glBlendFunc GL_ONE GL_ONE)
  (image-render %%genotype-framebuffer-img1)
  
  (glPopAttrib)
  (image-read-gl-pixels! %%genotype-current-image)
  (%%sum-fitness (image-bytes %%genotype-current-image)
                 (* (image-width %%genotype-current-image)
                    (image-height %%genotype-current-image)
                    4)))

;; Renders the genotype, subtracts the source image, and sums all of
;; the RGB values for the final fitness.
(define (calculate-fitness source-image gt)
  (if (not (eq? (image-format source-image) FORMAT_RGBA))
      (error "Source image must be in the RGBA format" source-image))
  
  (glClear GL_COLOR_BUFFER_BIT)
  (render-genotype gt)
  (image-read-gl-pixels! %%genotype-current-image)
      
  (%%calculate-fitness (image-bytes %%genotype-current-image)
                       (image-bytes source-image)
                       (* (image-width source-image)
                            (image-height source-image)
                            4)))


;; Population

(define (make-population size)
  (declare (fixnum))
  (let loop ((acc '())
             (i 0))
    (if (< i size)
        (loop (cons (random-genotype) acc)
              (+ i 1))
        acc)))

;; Normalizes genotype's fitness into something meaningful. Currently
;; it does this by finding the worst solution, and subtracting every
;; fitness from it. This inverts the scale and leaves the worst
;; solution with a fitness of 0.
(define (population-normalize pop)
  (let ((best-fitness (genotype-fitness
                       (population-fitness-search pop <)))
        (worst-fitness (genotype-fitness
                        (population-fitness-search pop >))))
    (let loop ((pop pop))
      (if (not (null? pop))
          (let ((gt (car pop)))
            (genotype-fitness-set! gt (- worst-fitness
                                           (genotype-fitness gt)))
            (loop (cdr pop)))))))

(define (population-fitness-search pop op)
  (fold (lambda (el acc)
          (if (op (genotype-fitness el)
                  (genotype-fitness acc))
              el
              acc))
        (car pop)
        (cdr pop)))

(define (population-run! pop source-image)
  (for-each (lambda (el)
              (genotype-fitness-set! el (run-genotype el source-image))
              ;;(pp (genotype-fitness el))
              )
            pop)
  (population-normalize population))

;; Drives the evolution cycle, using elitist selection and running
;; full genotype mutation on all but one (one is pristinely preserved)
(define (population-evolve pop)
  (let* ((count (length pop))
         (lst (selection-elitist pop count)))
    (let loop ((acc (list (car lst)))
               (tail (cdr lst)))
      (if (null? tail)
          acc
          (let ((head (car tail)))
            (case (random-integer 3)
              ((0) (mutate-genotype! head))
              ((1) (mutate-geometry! head))
              ((2) (mutate-polygons! head)))
            (loop (cons head acc)
                  (cdr tail)))))))

;; Special case evolution. We enforce a population size of 3, leaving
;; the first one a cloned copy, mutating only the polygons of the
;; second, and mutating only the geometry of the third.
(define (population-evolve-three pop)
  (let* ((lst (selection-elitist pop 3))
         (polygoner (cadr lst))
         (genotyper (caddr lst)))
    (mutate-polygons! polygoner) ; only mutate the polygons
    (mutate-geometry! genotyper) ; only do stuff like add/remove
                                 ; polygons
    lst))


;; Selection procedures
;;
;; SELECTION-ELITIST is the only used selection procedure.
;;
;; It was quickly found that a pure elitist selection was the only way
;; for the algorithm to converge.  The roulette wheel selection and
;; stochastic universal sampling procedures are not used anymore.

(define (selection-elitist pop n)
  (let ((best (population-fitness-search pop >)))
    (unfold (lambda (i) (>= i n))
            (lambda (i) (genotype-shallow-copy best))
            (lambda (i) (+ i 1))
            0)))

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
;;
;; These procedures are NOT used in this genetic algorithm.
;;
;; It was quickly found that this genetic algorithm only benefits from
;; slight mutations and no crossovers. These crossover operations were
;; initially implemented and are kept here for possible future uses.

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
;;
;; This is the crux of the algorithm. All of the actual mutation
;; operators are defined in the settings file (settings.scm). We set
;; up a harness for automatically running types mutators:
;; polygon, geometry, and genotype mutators.
;;
;; Polygon mutators are run across all polygons contained in a
;; genotype, and will mutate individual attributes of each polygon
;; according to the mutation rates.
;;
;; Geometry mutators are run on a single genotype and change various
;; parts of the a genotype's solution, such as adding/removing
;; polygons.
;;
;; Genotype mutators run both polygon and geometry mutators on a
;; single genotype.
;;
;; The settings file defines the two former mutator sets in the variables
;; `poly-mutators' and `geo-mutators'.

(define (run-mutators lst thing)
  (let loop ((tail lst))
    (if (not (null? tail))
        (let ((mutator (car tail))
              (prob (cadr tail)))
          (if (fl< (random-real) prob)
              (begin
                (if trace-mutators?
                    (begin (display mutator)
                           (newline)))
                (mutator thing)))
          (loop (cddr tail))))))

(define (mutate-polygon! polygon)
  (run-mutators poly-mutators polygon))

(define (mutate-polygons! gt)
  (genotype-data-set!
   gt
   (polygons-mutate-many (genotype-data gt))))

(define (mutate-geometry! gt)
  (run-mutators geo-mutators gt))

(define (mutate-genotype! gt)
  (mutate-polygons! gt)
  (mutate-geometry! gt))

(define (polygons-mutate-many polys)
  (let loop ((acc '())
             (data polys))
    (if (null? data)
        (reverse acc)
        (loop (cons (let* ((poly (car data))
                           (new-poly (make-polygon (polygon-points poly)
                                                   (polygon-red poly)
                                                   (polygon-green poly)
                                                   (polygon-blue poly)
                                                   (polygon-alpha poly))))
                      (mutate-polygon! new-poly)
                      new-poly)
                    acc)
              (cdr data)))))

;; Unused
(define (polygon-mutate-one polys)
  (let* ((polys (list->vector polys))
         (idx (random-integer (vector-length polys)))
         (poly (vector-ref polys idx))
         (new-poly (make-polygon (polygon-points poly)
                                 (polygon-red poly)
                                 (polygon-green poly)
                                 (polygon-blue poly)
                                 (polygon-alpha poly))))
    (mutate-polygon! new-poly)
    (vector-set! polys idx new-poly)
    (vector->list polys)))
