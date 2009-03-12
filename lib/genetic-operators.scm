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
  (vec2-add point (make-vec2 (* (- (random-real) .5) 100)
                             (* (- (random-real) .5) 100))))

(define (mutate-real real)
  (saturate (+ (* (half-negate (random-real)))
               real)))

(define-type mutator
  func
  weight)

(define mutators
  (list (make-mutator
         (lambda (tri)
           (triangle-point1-set! tri
            (mutate-point (triangle-point1 tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-point2-set! tri
            (mutate-point (triangle-point2 tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-point3-set! tri
            (mutate-point (triangle-point3 tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-red-set! tri
            (mutate-real (triangle-red tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-green-set! tri
            (mutate-real (triangle-green tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-blue-set! tri
            (mutate-real (triangle-blue tri))))
         1)
        (make-mutator
         (lambda (tri)
           (triangle-alpha-set! tri
            (mutate-real (triangle-alpha tri))))
         1)))

(define mutator-count (length mutators))

(define (mutate-triangle! triangle)
  (let ((mutator (selection-rws mutators
                                (random-integer mutator-count)
                                mutator-weight)))
    ((mutator-func mutator) triangle)))

(define (genotype-mutate gt #!optional rate)
  (make-genotype
   (let loop ((acc '())
              (data (genotype-data gt)))
     (if (null? data)
         (reverse acc)
         (loop (cons (if (< (random-real)
                            (or rate default-mutation-rate))
                         (let* ((tri (car data))
                                (new-tri (make-triangle (triangle-point1 tri)
                                                        (triangle-point2 tri)
                                                        (triangle-point3 tri)
                                                        (triangle-red tri)
                                                        (triangle-green tri)
                                                        (triangle-blue tri)
                                                        (triangle-alpha tri))))
                           (mutate-triangle! new-tri)
                           new-tri)
                         (car data))
                     acc)
               (cdr data))))))
