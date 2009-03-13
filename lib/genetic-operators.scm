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
  (let ((rand (random-integer 2))
        (point (make-vec2 (vec2-x point) (vec2-y point))))
    (if (= rand 0)
        (vec2-x-set! point (+ (vec2-x point) (* (- (random-real) .5) 10)))
        (vec2-y-set! point (+ (vec2-y point) (* (- (random-real) .5) 10))))
    point))

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
           (polygon-red-set! poly
            (mutate-real (polygon-red poly))))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-green-set! poly
            (mutate-real (polygon-green poly))))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-blue-set! poly
            (mutate-real (polygon-blue poly))))
         1)
        (make-mutator
         (lambda (poly)
           (polygon-alpha-set! poly
            (mutate-real (polygon-alpha poly))))
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
