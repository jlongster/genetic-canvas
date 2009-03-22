
(declare (block)
         (standard-bindings)
         (extended-bindings))

(load (resource lib-path "geometry"))

;; Genotype

(define-type genotype
  id: 6446D535-85C7-45DE-B3C7-32A2E2CD0658
  constructor: really-make-genotype
  data
  fitness)

(define (make-genotype data #!optional fitness)
  (really-make-genotype data (or fitness 0)))

(define (genotype-shallow-copy gt)
  (make-genotype (genotype-data gt) 0))

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
  (make-genotype
   (let loop ((acc '())
              (i 0))
     (if (< i 0)
         (loop (cons (random-polygon) acc)
               (+ i 1))
         acc))
   0.0))

(define (render-genotype gt)
  (let loop ((tris (genotype-data gt)))
    (if (not (null? tris))
        (begin
          (render-polygon (car tris))
          (loop (cdr tris))))))

(define %%genotype-current-image #f)

(define (run-genotype gt source-image)
  ;; Allocate an image buffer if necessary
  (if (or (not %%genotype-current-image)
          (not (eq? (image-width source-image)
                    (image-width %%genotype-current-image)))
          (not (eq? (image-height source-image)
                    (image-height %%genotype-current-image))))
      (set! %%genotype-current-image
            (make-image (image-width source-image)
                        (image-height source-image)
                        FORMAT_RGBA)))

  ;; Draw genotype
  (glClear GL_COLOR_BUFFER_BIT)
  (render-genotype gt)
  (image-read-gl-pixels! %%genotype-current-image)
  (calculate-fitness source-image %%genotype-current-image))

(define (calculate-fitness source-image image)

  (define (byte-difference bytes1 bytes2 i)
    (abs (- (u8*-ref bytes1 i)
            (u8*-ref bytes2 i))))
  
  (if (not (eq? (image-format source-image) FORMAT_RGBA))
      (error "Source image must be in the RGBA format" source-image))
  (if (not (eq? (image-format image) FORMAT_RGBA))
      (error "Input image must be in the RGBA format" image))

  (let ((source-bytes (image-bytes source-image))
        (bytes (image-bytes image))
        (width (image-width image))
        (height (image-height image)))
    (let loop ((acc 0)
               (i 0))
      (if (< i (* width height 4))
          (loop (+ acc
                   (byte-difference bytes source-bytes i)
                   (byte-difference bytes source-bytes (+ i 1))
                   (byte-difference bytes source-bytes (+ i 2)))
                (+ i 4))
          acc))))

(define (overall-fitness fitness)
  ; fitness is the total amount of difference
  ; in the image
  (- 1.0
     (/ fitness (* (current-width)
                   (current-height)
                   255
                   3))))


;; Population

(define (make-population size)
  (let loop ((acc '())
             (i 0))
    (if (< i size)
        (loop (cons (random-genotype) acc)
              (+ i 1))
        acc)))

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
  (fold (lambda (el acc)
          (genotype-fitness-set! el (run-genotype el source-image)))
        #f
        pop)
  (population-normalize population))

(define (population-evolve pop)
  (let* ((count (length pop))
         (best (population-fitness-search pop >))
         (pop (unfold (lambda (i) (>= i count))
                      (lambda (i) (genotype-shallow-copy (car sorted)))
                      (lambda (i) (+ i 1))
                      0)))
    (let loop ((acc '())
               (tail pop))
      (cond
       ((null? tail) acc)
       ((eq? (cdr tail) '())
        (reverse (cons (genotype-mutate-one (car tail)) acc)))
       (else
        (let ((gt1 (car tail))
              (gt2 (cadr tail))
              (tail (cddr tail)))
          (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2)
            (loop (cons (genotype-mutate-one new-gt2)
                        (cons (genotype-mutate-one new-gt1) acc))
                  tail))))))))

(define (population-evolve-three pop)
  (if (not (eq? (length pop) 3))
      (error "population size must be 3"))
  (let ((best (population-fitness-search pop >)))
    (list (genotype-shallow-copy best)  ; clone it
          (let ((gt (genotype-shallow-copy best)))
            (mutate-polygons! gt)       ; only mutate the polygons
            gt)
          (let ((gt (genotype-shallow-copy best)))
            (mutate-genotype! gt)       ; only do stuff like
                                        ; add/remove polygons
            gt))))


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
;;
;; These procedures are not used in this genetic program. They were
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
;; This is the crux of the algorithm. The success of the genetic
;; learning highly depends on the implementation and configuration of
;; the following several mutation operators.

(define (scale-negation f)
  (- (* f 2.) 1.))

(define (random-real-in-range minv maxv)
  (+ (* (random-real)
        (- maxv minv))
     minv))

(define (mutate-point point)
  (make-vec2 (+ (vec2-x point) (* (scale-negation (random-real)) 20.))
             (+ (vec2-y point) (* (scale-negation (random-real)) 20.))))

(define (mutate-real real minv maxv)
  (min (max (+ (* (random-real-in-range -.1 .1)) real)
            minv)
       maxv))

(define-type mutator
  name
  func
  probability)

;; add a "move polygon"?
(define poly-mutators
  (list ;; (make-mutator 'add-point
;;          (lambda (poly)
;;            (polygon-points-set! poly
;;             (cons (random-point) (polygon-points poly))))
;;          .001)
;;         (make-mutator 'remove-point
;;          (lambda (poly)
;;            (if (> (length (polygon-points poly)) 3)
;;                (begin
;;                  (polygon-points-set!
;;                   poly
;;                   (cdr (polygon-points poly))))))
;;          .001)
        (make-mutator 'move-point-minor
         (lambda (poly)
           (let* ((points (list->vector (polygon-points poly)))
                  (idx (random-integer (vector-length points))))
             (vector-set! points idx
                          (mutate-point (vector-ref points idx)))
             (polygon-points-set! poly (vector->list points))))
         .1)
        (make-mutator 'change-red
         (lambda (poly)
           (polygon-red-set! poly (random-real-in-range min-red max-red)))
         .001)
        (make-mutator 'change-red-minor
         (lambda (poly)
           (polygon-red-set! poly (mutate-real (polygon-red poly)
                                               min-red
                                               max-red)))
         .05)
        (make-mutator 'change-green
         (lambda (poly)
           (polygon-green-set! poly (random-real-in-range min-green max-green)))
         .001)
        (make-mutator 'change-green-minor
         (lambda (poly)
           (polygon-green-set! poly (mutate-real (polygon-green poly)
                                                 min-green
                                                 max-green)))
         .05)
        (make-mutator 'change-blue
         (lambda (poly)
           (polygon-blue-set! poly (random-real-in-range min-blue max-blue)))
         .001)
        (make-mutator 'change-blue-minor
         (lambda (poly)
           (polygon-blue-set! poly (mutate-real (polygon-blue poly)
                                                min-blue
                                                max-blue)))
         .05)
        (make-mutator 'change-alpha
         (lambda (poly)
           (polygon-alpha-set! poly (+ (* (random-real) .7) .1)))
         .001)
        (make-mutator 'change-alpha-minor
         (lambda (poly)
           (polygon-alpha-set! poly
                               (mutate-real
                                (polygon-alpha poly) 0. 1.)))
         .05)))

(define genotype-mutators
  (list (make-mutator 'remove-poly
         (lambda (gt)
           (if (not (null? (genotype-data gt)))
               (genotype-data-set!
                gt
                (let loop ((acc '())
                           (tail (genotype-data gt))
                           (i 0)
                           (rnd (random-integer (length (genotype-data gt)))))
                  (if (null? tail)
                      (reverse acc)
                      (if (= i rnd)
                          (loop acc (cdr tail) (+ i 1) rnd)
                          (loop (cons (car tail) acc)
                                (cdr tail)
                                (+ i 1)
                                rnd)))))))
         .05)
        (make-mutator 'add-poly
         (lambda (gt)
           (genotype-data-set!
            gt
            (cons (random-polygon)
                  (genotype-data gt))))
         .7)))

(define (run-mutators lst thing)
  (let loop ((tail lst))
    (if (not (null? tail))
        (let ((m (car tail)))
          (if (< (random-real) (mutator-probability m))
              (begin
                (display (mutator-name m)) (newline)
                ((mutator-func m) thing)))
          (loop (cdr tail))))))

(define (mutate-polygon! polygon)
  (run-mutators poly-mutators polygon))

(define (mutate-polygons! gt)
  (genotype-data-set!
   gt
   (polygons-mutate-many (genotype-data gt))))

(define (mutate-genotype! gt)
  (run-mutators genotype-mutators gt))

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
