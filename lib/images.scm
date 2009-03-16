
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "ffi/util.scm")

;; Images

(define FORMAT_RGB GL_RGB)
(define FORMAT_RGBA GL_RGBA)
(define FORMAT_LUMINANCE GL_LUMINANCE)

(define-type image
  id: EF8C0406-A5C4-48A6-BC1B-B615E1FEEFA6
  width
  height
  bytes
  format
  gl-texture-id)

(define (load-image filename)
  (if (not (or (not (eq? (string-suffix-length filename ".jpg") 0))
               (not (eq? (string-suffix-length filename ".jpeg") 0))))
      (raise '("Image type not supported", filename)))  
  (let ((img (freeimage-load-jpg filename)))
    (make-image (freeimage-width img)
                (freeimage-height img)
                (freeimage-bytes img)
                FORMAT_RGB
                #f)))

(define (image-opengl-upload! image)
  (let ((tex (with-alloc (buf (alloc-uint 1))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  (image-format image)
                  (image-width image)
                  (image-height image)
                  0
                  (image-format image)
                  GL_UNSIGNED_BYTE
                  (u8*->void* (image-bytes image)))
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MIN_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MAG_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_S
                     GL_REPEAT)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_T
                     GL_REPEAT)
    (glBindTexture GL_TEXTURE_2D 0)
    (image-gl-texture-id-set! image tex)))

(define (image-fetch-rgb image x y)
  (let ((x (min x (- (image-width image) 1)))
        (y (min y (- (image-height image) 1))))
    (let* ((offset (+ (* y (image-width image)) x))
           (byte-offset (* offset 3))
           (data (image-bytes image)))
      (make-vec3 (char->integer (u8*-ref data byte-offset))
                 (char->integer (u8*-ref data (+ byte-offset 1)))
                 (char->integer (u8*-ref data (+ byte-offset 2)))))))

(define (image-render image)
  (glBindTexture GL_TEXTURE_2D (image-gl-texture-id source-image))
  (glBegin GL_QUADS)
  (begin
    (glTexCoord2d 0. 1.)
    (glVertex2f 0. 0.))
  (begin
    (glTexCoord2d 0. 0.)
    (glVertex2f 0. (current-height)))
  (begin
    (glTexCoord2d 1. 0.)
    (glVertex2f (current-width)
                (current-height)))
  (begin
    (glTexCoord2d 1. 1.)
    (glVertex2f (current-width) 0.))
  (glEnd)
  (glBindTexture GL_TEXTURE_2D 0))


;; Analzying an image, edge detection, etc.

(define (find-points-on-edges image)
  (let ((width (image-width image))
        (height (image-height image))
        (bytes (image-bytes image)))
    (let loop ((points '())
               (i 0))
      (if (< i (* width height))
          (if (> (byte->real (u8*-ref bytes i)) .001)
              (loop (cons (make-vec2 (exact->inexact (/ (remainder i width) width))
                                     (exact->inexact (/ (quotient i width) height)))
                          points)
                    (+ i 1))
              (loop points (+ i 1)))
          (list->vector points)))))

(define gaussian-kernel-edge-length 5)
(define gaussian-kernel
  (vector
   1 4  7  4  1
   4 16 26 16 4
   7 26 41 26 7
   4 16 26 16 4
   1 4  7  4  1))

(define edge-kernel-edge-length 5)
(define edge-kernel
  (vector
   -1 -1 -1 -1 -1
   -1  0  0  0 -1
   -1  0 16  0 -1
   -1  0  0  0 -1
   -1 -1 -1 -1 -1))

(define (rgb->greyscale bytes length)
  (let ((output (alloc-u8 length)))
    (let loop ((i 0))
      (if (< i length)
          (let ((r (byte->real (u8*-ref bytes (* i 3))))
                (g (byte->real (u8*-ref bytes (+ (* i 3) 1))))
                (b (byte->real (u8*-ref bytes (+ (* i 3) 2)))))
            (u8*-set! output i (real->byte
                                (+ (* r .3)
                                   (* g .59)
                                   (* b .11))))
            (loop (+ i 1)))
          output))))

(define (make-edges-white! bytes width height)
  (let loop ((i 0))
    (if (< i (* width height))
        (let ((x (remainder i width))
              (y (quotient i width)))
          (if (or (< x 5)
                  (< y 5)
                  (> x (- width 5))
                  (> y (- height 5)))
              (u8*-set! bytes i
                        (integer->char 255)))
          (loop (+ i 1))))))

(define (apply-filter filter filter-edge-length bytes x y width height)
  (let ((offset (/ (- filter-edge-length 1) 2))
        (filter-size (* filter-edge-length filter-edge-length)))
    (let loop ((value 0)
               (i 0)
               (applied 0))
      (if (< i filter-size)
          (let* ((kernel-x (remainder i filter-edge-length))
                 (kernel-y (quotient i filter-edge-length))
                 (img-x (+ (- x offset) kernel-x))
                 (img-y (+ (- y offset) kernel-y)))
            (if (or (< img-x 0)
                    (< img-y 0))
                (loop value (+ i 1) applied)
                (loop (+ value (* (vector-ref filter i)
                                  (byte->real (u8*-ref bytes
                                                       (+ (* width img-y)
                                                          img-x)))))
                      (+ i 1)
                      (+ applied 1))))
          (* value (/ applied filter-size))))))

(define-macro (define-filter name kernel kernel-edge-length normalize)
  `(define (,name bytes width height)
     (let* ((length (* width height))
            (output (alloc-u8 length)))
       (let loop ((i 0))
         (if (< i length)
             (let ((x (remainder i width))
                   (y (quotient i width)))
               (u8*-set! output i
                         (,normalize
                          (apply-filter ,kernel ,kernel-edge-length
                                        bytes x y width height)))
               (loop (+ i 1)))
             output)))))

(define-filter gaussian-blur-filter
  gaussian-kernel
  gaussian-kernel-edge-length
  (lambda (value)
    (real->byte (saturate (/ value 273.)))))

(define-filter edge-filter
  edge-kernel
  edge-kernel-edge-length
  (lambda (value)
    (real->byte (if (< value .5) 0. 1.))))


;; Utility

(define (saturate f)
  (if (< f 0.) 0.
      (if (> f 1.) 1.
          f)))

(define (byte->real byte)
  (/ (char->integer byte) 255))

(define (real->byte f)
  (integer->char
   (inexact->exact
    (floor (* (saturate f) 255)))))
