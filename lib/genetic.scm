
(declare (block)
         (standard-bindings)
         (extended-bindings))

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

(define (copy-genotype gt)
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

(define (polygon-equal? poly1 poly2)
  (let ((points1 (polygon-points poly1))
        (points2 (polygon-points poly2))
        (color-same (color-equal? poly1 poly2)))
    (let loop ((tail1 points1)
               (tail2 points2)
               (same #t))
      (if (or (not same)
              (null? tail1)
              (null? tail2))
          (and same color-same)
          (loop (cdr tail1)
                (cdr tail2)
                (point-equal? (car tail1)
                              (car tail2)))))))

(define (point-equal? p1 p2)
  (vec2-equal? p1 p2))

(define (color-equal? poly1 poly2)
  (and (eq? (polygon-red poly1) (polygon-red poly2))
       (eq? (polygon-green poly1) (polygon-green poly2))
       (eq? (polygon-blue poly1) (polygon-blue poly2))))

(define global-point-field #f)

(define (random-point)
  (if global-point-field
      (let ((point (vector-ref global-point-field
                               (random-integer
                                (vector-length global-point-field)))))
        (make-vec2 (* (vec2-x point) (current-width))
                   (* (vec2-y point) (current-height))))
      (make-vec2
       (* (random-real)
          (exact->inexact (current-width)))
       (* (random-real)
          (exact->inexact (current-height))))))

(define (random-polygon)
  (let ((origin (random-point)))
    (make-polygon
     (unfold (lambda (i) (>= i 3))
             (lambda (i) (vec2-add origin
                                   (make-vec2 (- (random-integer 30) 15)
                                              (- (random-integer 30) 15))))
             (lambda (i) (+ i 1))
             0)
     (random-real)
     (random-real)
     (random-real)
     (* (random-real) .6))))

(define (random-genotype)
  (make-genotype
   (let loop ((acc '())
              (i 0))
     (if (< i 0)
         (loop (cons (random-polygon) acc)
               (+ i 1))
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

  (let* ((widthi (inexact->exact (current-width)))
         (heighti (inexact->exact (current-height))))
    (glReadPixels 0 0
                  widthi heighti
                  GL_RGBA GL_UNSIGNED_BYTE
                  (u8*->void* texture-buffer))
    (calculate-fitness texture-buffer)))

(define (byte-difference bytes1 bytes2 i)
  (abs (- (char->integer (u8*-ref bytes1 i))
          (char->integer (u8*-ref bytes2 i)))))

(define (calculate-fitness bytes)
  (let ((source-bytes (image-bytes source-image-sized))
        (width (image-width source-image-sized))
        (height (image-height source-image-sized)))
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
  (- 1.0
     (/ fitness (* (current-width)
                   (current-height)
                   255
                   3))))


;; Population

(define (make-population size #!optional point-field)
  (set! global-point-field point-field)
  (let loop ((acc '())
             (i 0))
    (if (< i size)
        (loop (cons (random-genotype) acc)
              (+ i 1))
        acc)))

(define (population-normalize pop)
  (let ((best-fitness (genotype-fitness
                       (population-fitness-search pop <)))
        (worst-fitnes (genotype-fitness
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

(define (population-run! pop)
  (fold (lambda (el acc)
          (genotype-fitness-set! el (run-genotype el)))
        #f
        pop)
  (population-normalize population))

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
              (reverse (cons (genotype-mutate-one (car tail)) acc)))
             (else
              (let ((gt1 (car tail))
                    (gt2 (cadr tail))
                    (tail (cddr tail)))
                (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2)
                  (loop (cons (genotype-mutate-one new-gt2)
                              (cons (genotype-mutate-one new-gt1) acc))
                        tail)))))))))

(define (population-evolve-three! pop)
  (if (not (eq? (length pop) 3))
      (error "population-evolve-three!: population size must be 3"))
  (let ((best (fold (lambda (el acc)
                      (if (> (genotype-fitness el)
                             (genotype-fitness acc))
                          el acc))
                    (car pop)
                    (cdr pop))))
    (set! population
          (list (copy-genotype best)
                (let ((gt (copy-genotype best)))
                  (mutate-polygons! gt)
                  gt)
                (let ((gt (copy-genotype best)))
                  (mutate-genotype! gt)
                  gt)))))


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

(define (half-negate f)
  (- (* f 2.) 1.))

(define (mutate-point point)
  (make-vec2 (+ (vec2-x point) (* (half-negate (random-real)) 20))
             (+ (vec2-y point) (* (half-negate (random-real)) 20))))

(define (mutate-real real minv maxv)
  (min (max (+ (* (random-real-in-range -.1 .1)) real)
            minv)
       maxv))

(define (random-real-in-range minv maxv)
  (+ (* (random-real)
        (- maxv minv))
     minv))

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
;;         (make-mutator 'move-point
;;          (lambda (poly)
;;            (let* ((points (list->vector (polygon-points poly)))
;;                   (idx (random-integer (vector-length points))))
;;              (vector-set! points idx (random-point))
;;              (polygon-points-set! poly (vector->list points))))
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
  (run-mutators genotype-mutators gt)
  ;; (genotype-data-set!
;;    gt
;;    (polygons-mutate-many (genotype-data gt)))
  )

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
