
(define lib-path "lib")
(define resource-path "resources")

(define (path-trim str)
  (let ((new-str str))
    (if (eq? (string-ref new-str 0)
             #\/)
        (set! new-str
              (substring new-str
                         1 (string-length new-str))))
    (if (eq? (string-ref new-str (- (string-length new-str) 1))
             #\/)
        (set! new-str
              (substring new-str
                         0 (- (string-length new-str) 1))))
    new-str))

(define (join-paths . args)
  (let loop ((path (if (eq? (string-ref (car args) 0) #\/)
                       "/" ""))
             (head (car args))
             (tail (cdr args)))
    (let ((new-path (string-append path (path-trim head) "/")))
      (if (null? tail)
          (if (eq? (string-ref head (- (string-length head) 1))
                   #\/)
              new-path
              (substring new-path 0 (- (string-length new-path) 1)))
          (loop new-path
                (car tail)
                (cdr tail))))))

(define (resource . args)
  (apply
   join-paths
   (cons "/Users/james/projects/scheme/artbot/"
         args)))
