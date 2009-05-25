
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

(define (random-points)
  (let ((origin (random-point))
        (num-points 3)
        (scale (random-integer 50)))
    (unfold (lambda (i) (>= i num-points))
            (lambda (i)
              (vec2-add
               origin
               (make-vec2 (* scale (scale-negation (random-real)))
                          (* scale (scale-negation (random-real))))))
            (lambda (i) (+ i 1))
            0)))

(define (random-polygon)
  (make-polygon
   (random-points)
   (random-real)
   (random-real)
   (random-real)
   (* (random-real) .6)))

(define (render-polygon poly #!optional (border #f))
  (define (%%render)
    (glBegin GL_POLYGON)
    (let loop ((tail (polygon-points poly)))
      (if (not (null? tail))
          (let ((point (car tail)))
            (glVertex2f (vec2-x point) (vec2-y point))
            (loop (cdr tail)))))
    (glEnd))

  (glColor4f (polygon-red poly)
             (polygon-green poly)
             (polygon-blue poly)
             (polygon-alpha poly))
  (%%render)

  (if border
      (begin
        (glPushAttrib GL_POLYGON_BIT)
        (glPolygonMode GL_FRONT_AND_BACK GL_LINE)
        (glColor4f 1. 1. 1. 1.)
        (%%render)
        (glPopAttrib))))

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
