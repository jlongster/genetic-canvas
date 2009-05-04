;; settings, the module which controls the genetic algorithm.
;;
;; Here's how this works. The genetic algorithm is implemented in
;; genetic.scm, but we don't want to mess around with all that code
;; just to tweak how the algorithm runs. Therefore, this file was
;; created to implement the important parts of the genetic algorithm:
;; the parts which decide when and what to mutate, and a few other
;; minor settings.

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; This is run when the application starts

(define (configure image)
  (random-source-randomize! default-random-source)
  (configure-colors image)
  (configure-start-color image))

;; These settings configure the algorithm and affect the initial
;; solution

(define source-image-file "resources/test2.jpg")
(define hardware-accelerated? #f)
(define trace-mutators? #f)
(define blur-image? #t)
 
(define population-size 10)
(define initial-num-polygons 0)
(define initial-color (make-vec3 0. 0. 0.))

(define min-red 0.)
(define max-red 1.)
(define min-green 0.)
(define max-green 1.)
(define min-blue 0.)
(define max-blue 1.)

(define (configure-colors image)
  (if (not (eq? (image-format image) FORMAT_RGBA))
      (error "Unsupported image type for configure-colors"))
  (let ((bytes (image-bytes image)))
    (let loop ((min-r 1.)
               (min-g 1.)
               (min-b 1.)
               (max-r 0.)
               (max-g 0.)
               (max-b 0.)
               (i 0))
      (if (< i (* (image-width image)
                  (image-height image)
                  4))
          (let ((r (byte->real (u8*-ref bytes i)))
                (g (byte->real (u8*-ref bytes (+ i 1))))
                (b (byte->real (u8*-ref bytes (+ i 2)))))
            (loop (flmin min-r r)
                  (flmin min-g g)
                  (flmin min-b b)
                  (flmax max-r r)
                  (flmax max-g g)
                  (flmax max-b b)
                  (+ i 4)))
          (begin
            (set! min-red min-r)
            (set! max-red max-r)
            (set! min-green min-g)
            (set! max-green max-g)
            (set! min-blue min-b)
            (set! max-blue max-b))))))

(define (configure-start-color image)
  (define (inc vec i)
    (vector-set! vec i (+ (vector-ref vec i) 1)))

  (define (max-in-vector vec)
    ;; I really need to look into foof-loop
    ;; or better looping procedures.  I'm doing
    ;; this clunky method all the time.
    (let loop ((best 0)
               (idx 0)
               (i 0))
      (if (>= i (vector-length vec))
          idx
          (if (> (vector-ref vec i) best)
              (loop (vector-ref vec i) i (+ i 1))
              (loop best idx (+ i 1))))))
  
  (if (not (eq? (image-format image) FORMAT_RGBA))
      (error "Unsupported image type for configure-start-color"))
  (let ((bytes (image-bytes image))
        (len (* (image-width image)
                (image-height image)
                4))
        (rstat (make-vector 256))
        (gstat (make-vector 256))
        (bstat (make-vector 256)))
    (let loop ((i 0))
      (if (>= i len)
          (let ((r (max-in-vector rstat))
                (g (max-in-vector gstat))
                (b (max-in-vector bstat)))
            (set! initial-color (make-vec3 (byte->real r)
                                           (byte->real g)
                                           (byte->real b))))
          (begin
            (inc rstat (u8*-ref bytes i))
            (inc gstat (u8*-ref bytes (+ i 1)))
            (inc bstat (u8*-ref bytes (+ i 2)))
            (loop (+ i 4)))))))

;; Util

(define (scale-negation f)
  (- (* f 2.) 1.))

(define (random-real-in-range minv maxv)
  (+ (* (random-real)
        (- maxv minv))
     minv))

(define (permutation-vector #!optional scale)
  (make-vec2 (* (scale-negation (random-real)) (or scale 5.))
             (* (scale-negation (random-real)) (or scale 5.))))

(define (mutate-point point scale)
  (vec2-add point (permutation-vector scale)))

(define (mutate-real real minv maxv)
  (min (max (+ (* (random-real-in-range -.04 .04)) real)
            minv)
       maxv))

;; Polygon mutators
;;
;; These mutators tweak attributes of a single polygon

(define-macro (make-mutators . lst)
  `(list ,@lst))

(define (add-point poly)
  (let* ((points (list->vector (polygon-points poly)))
         (idx (random-integer (vector-length points)))
         (p1 (vector-ref points (modulo (- idx 1)
                                        (vector-length points))))
         (p2 (vector-ref points idx))
         (new-point (make-vec2 (/ (+ (vec2-x p1) (vec2-x p2)) 2)
                               (/ (+ (vec2-y p1) (vec2-y p2)) 2))))
    (let ((new-points
           (let loop ((acc '())
                      (i 0))
             (if (>= i (vector-length points))
                 (reverse acc)
                 (if (= i idx)
                     (loop (cons (vector-ref points i)
                                 (cons new-point acc))
                           (+ i 1))
                     (loop (cons (vector-ref points i) acc)
                           (+ i 1)))))))
      (polygon-points-set! poly new-points))))

(define (remove-point poly)
  (let* ((points (list->vector (polygon-points poly)))
         (idx (random-integer (vector-length points))))
    (if (> (vector-length points) 3)
        (let ((new-points
               (let loop ((acc '())
                          (i 0))
                 (if (>= i (vector-length points))
                     (reverse acc)
                     (if (= i idx)
                         (loop acc (+ i 1))
                         (loop (cons (vector-ref points i) acc) (+ i 1)))))))
          (polygon-points-set! poly new-points)))))

(define (move-point-minor poly)
  (let* ((points (list->vector (polygon-points poly)))
         (idx (random-integer (vector-length points))))
    (vector-set! points idx
                 (mutate-point (vector-ref points idx) 5.))
    (polygon-points-set! poly (vector->list points))))

(define (move-point-more poly)
  (let* ((points (list->vector (polygon-points poly)))
         (idx (random-integer (vector-length points))))
    (vector-set! points idx
                 (mutate-point (vector-ref points idx) 20.))
    (polygon-points-set! poly (vector->list points))))

(define (change-red poly)
  (polygon-red-set! poly (random-real-in-range min-red max-red)))

(define (change-red-minor poly)
  (polygon-red-set! poly (mutate-real (polygon-red poly)
                                      min-red
                                      max-red)))

(define (change-green poly)
  (polygon-green-set! poly (random-real-in-range min-green max-green)))

(define (change-green-minor poly)
  (polygon-green-set! poly (mutate-real (polygon-green poly)
                                        min-green
                                        max-green)))

(define (change-blue poly)
  (polygon-blue-set! poly (random-real-in-range min-blue max-blue)))

(define (change-blue-minor poly)
  (polygon-blue-set! poly (mutate-real (polygon-blue poly)
                                       min-blue
                                       max-blue)))

(define (change-alpha poly)
  (polygon-alpha-set! poly (+ (* (random-real) .25) .11)))

(define (change-alpha-minor poly)
  (polygon-alpha-set! poly
                      (mutate-real
                       (polygon-alpha poly) .25 .11)))

(define poly-mutators
  (make-mutators
   add-point .003
   remove-point .003
   move-point-minor .007
   move-point-more .007
   change-red-minor .007
   change-red .002
   change-green-minor .007
   change-green .002
   change-blue-minor .007
   change-blue .002
   change-alpha-minor .007
   change-alpha .002))


;; Genotype mutators
;;
;; These mutators only affect poylgons in the genotype as a whole,
;; leaving specific attributes of each polygon untouched
(define (remove-poly gt)
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

(define (add-poly gt)
  (genotype-data-set!
   gt
   (cons (random-polygon)
         (genotype-data gt))))

(define (reorder-poly gt)
  (let* ((polys (list->vector (genotype-data gt)))
         (len (vector-length polys)))
    (if (> len 1)
        (let* ((idx1 (random-integer len))
               (idx2 (random-integer len))
               (poly (vector-ref polys idx1)))
          (genotype-data-set!
           gt
           (let loop ((acc '())
                      (i 0))
             (if (>= i len)
                 (reverse acc)
                 (cond
                  ((= i idx1) (loop acc (+ i 1)))
                  ((= i idx2) (loop (cons (vector-ref polys i)
                                          (cons poly acc))
                                    (+ i 1)))
                  (else (loop (cons (vector-ref polys i) acc)
                              (+ i 1)))))))))))

(define geo-mutators
  (make-mutators
   add-poly .005
   remove-poly .003
   reorder-poly .005))
