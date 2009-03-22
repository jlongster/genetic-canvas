
;; Polygon

(define-structure polygon
  points
  red
  green
  blue
  alpha)

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

(define (render-polygon poly)
  (glBegin GL_POLYGON)
  (glColor4f (polygon-red poly)
             (polygon-green poly)
             (polygon-blue poly)
             (polygon-alpha poly))
  (let loop ((tail (polygon-points poly)))
    (if (not (null? tail))
        (let ((point (car tail)))
          (glVertex2f (vec2-x point) (vec2-y point))
          (loop (cdr tail)))))
  (glEnd))

;; Point

(define (point-equal? p1 p2)
  (vec2-equal? p1 p2))

(define (random-point)
  (make-vec2
   (* (random-real)
      (exact->inexact (current-width)))
   (* (random-real)
      (exact->inexact (current-height)))))

;; Color

(define (color-equal? poly1 poly2)
  (and (eq? (polygon-red poly1) (polygon-red poly2))
       (eq? (polygon-green poly1) (polygon-green poly2))
       (eq? (polygon-blue poly1) (polygon-blue poly2))))
