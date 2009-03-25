;; Images
;;
;; This modules provides all sorts of functions for doing the painful
;; task of dealing with images and textures. There are procedures for
;; loading JPG images, uploading them as OpenGL textures, rendering
;; them, and even applying gaussian blurring and edge filters.

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "ffi/util.scm")

(define FORMAT_RGB GL_RGB)
(define FORMAT_RGBA GL_RGBA)
(define FORMAT_LUMINANCE GL_LUMINANCE)

(define-type image
  id: EF8C0406-A5C4-48A6-BC1B-B615E1FEEFA6
  constructor: really-make-image
  width
  height
  bytes
  format
  gl-texture-id)

(define (make-image width height format #!optional bytes)
  (let* ((bytes-count (cond
                        ((eq? format FORMAT_LUMINANCE) 1)
                        ((eq? format FORMAT_RGB) 3)
                        ((eq? format FORMAT_RGBA) 4)
                        (else (error "Invalid format" format))))
         (bytes (or bytes (alloc-u8 (* width height bytes-count))))
         (img (really-make-image width height bytes format #f)))
    (make-will img (lambda (x)
                     (free (image-bytes x))))
    img))

(define (load-image filename)
  (if (not (or (not (eq? (string-suffix-length filename ".jpg") 0))
               (not (eq? (string-suffix-length filename ".jpeg") 0))))
      (raise '("Image type not supported", filename)))  
  (let ((img (freeimage-load-jpg filename)))
    (make-image (freeimage-width img)
                (freeimage-height img)
                FORMAT_RGB
                (freeimage-bytes img))))

(define (image-read-gl-pixels! image)
  (if (not (eq? (image-format image) FORMAT_RGBA))
      (error (string-append "You can only read pixels from opengl into "
                            "an image with the format RGBA")
             image))
  (glReadPixels 0 0
                (image-width image)
                (image-height image)
                GL_RGBA GL_UNSIGNED_BYTE
                (u8*->void* (image-bytes image))))

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
      (make-vec3 (u8*-ref data byte-offset)
                 (u8*-ref data (+ byte-offset 1))
                 (u8*-ref data (+ byte-offset 2))))))

(define (image-render image #!optional width height)
  (if (not (image-gl-texture-id image))
      (image-opengl-upload! image))
  
  (let ((w (or width (image-width image)))
        (h (or height (image-height image))))
    (glBindTexture GL_TEXTURE_2D (image-gl-texture-id image))
    (glBegin GL_QUADS)
    (begin
      (glTexCoord2d 0. 1.)
      (glVertex2f 0. 0.))
    (begin
      (glTexCoord2d 0. 0.)
      (glVertex2f 0. (real h)))
    (begin
      (glTexCoord2d 1. 0.)
      (glVertex2f (real w) (real h)))
    (begin
      (glTexCoord2d 1. 1.)
      (glVertex2f (real w) 0.))
    (glEnd)
    (glBindTexture GL_TEXTURE_2D 0)))

(define (bytes-per-pixel format)
  (cond
   ((eq? format FORMAT_LUMINANCE) 1)
   ((eq? format FORMAT_RGB) 3)
   ((eq? format FORMAT_RGBA) 4)
   (else (error "Unsupported format"))))

(define (image-to-greyscale image)
  (make-image (image-width image)
              (image-height image)
              FORMAT_LUMINANCE
              (rgb->greyscale (image-bytes)
                              (* (image-width image)
                                 (image-height image)
                                 (bytes-per-pixel (image-format image))))))

(define (image-blur img)
  (cond
   ((eq? (image-format img) FORMAT_LUMINANCE)
    (gaussian-blur-filter (image-bytes img)
                          (image-width img)
                          (image-height img)))
   ((eq? (image-format img) FORMAT_RGB)
    (%%color-image-blur img))
   (else (error "Unsupported image format"))))

(define (strip-bytes img offset)
  (let* ((len (* (image-width img)
                 (image-height img)))
         (bytes (image-bytes img))
         (out (alloc-u8 len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (u8*-set! out i (u8*-ref bytes (+ (* i 3) offset)))
            (loop (+ i 1)))
          out))))

(define (strip-red-bytes img)
  (strip-bytes img 0))

(define (strip-green-bytes img)
  (strip-bytes img 1))

(define (strip-blue-bytes img)
  (strip-bytes img 2))

(define (weave-rgb-bytes r g b len)
  (let ((out (alloc-u8 (* len 3))))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (u8*-set! out (* i 3) (u8*-ref r i))
            (u8*-set! out (+ (* i 3) 1) (u8*-ref g i))
            (u8*-set! out (+ (* i 3) 2) (u8*-ref b i))
            (loop (+ i 1)))
          out))))

(define (%%color-image-blur img)
  (let* ((width (image-width img))
         (height (image-height img))
         (red (strip-red-bytes img))
         (green (strip-green-bytes img))
         (blue (strip-blue-bytes img))
         (red-blurred (gaussian-blur-filter red width height))
         (green-blurred (gaussian-blur-filter green width height))
         (blue-blurred (gaussian-blur-filter blue width height)))
    (let ((res (make-image (image-width img)
                           (image-height img)
                           FORMAT_RGB
                           (weave-rgb-bytes red-blurred
                                            green-blurred
                                            blue-blurred
                                            (* width height)))))
      (free red)
      (free green)
      (free blue)
      (free red-blurred)
      (free green-blurred)
      (free blue-blurred)
      res)))



;; Analzying an image, edge detection, etc.

(define (gaussian-distribution x y rho)
  (* (/ 1. (sqrt (* 2. 3.141592654 rho rho)))
     (exp (/ (- (+ (* x x)
                   (* y y)))
             (* 2 rho rho)))))

(define gaussian-kernel-edge-length 5)
(define gaussian-rho 1.2)
(define gaussian-kernel
  (let* ((len (* gaussian-kernel-edge-length
                 gaussian-kernel-edge-length))
         (offset (/ (- gaussian-kernel-edge-length 1) 2))
         (out (make-vector len)))
    (let loop ((i 0))
      (if (< i len)
          (let ((x (remainder i gaussian-kernel-edge-length))
                (y (quotient i gaussian-kernel-edge-length)))
            (vector-set! out i
                         (gaussian-distribution (- x offset)
                                                (- y offset)
                                                gaussian-rho))
            (loop (+ i 1)))
          out))))

(define edge-kernel-edge-length 5)
(define edge-kernel
  (vector
   -1 -1 -1 -1 -1
   -1  0  0  0 -1
   -1  0 16  0 -1
   -1  0  0  0 -1
   -1 -1 -1 -1 -1))

(define (print-kernel kernel edge-length)
  (let ((len (* edge-length edge-length)))
    (let loop ((i 0))
      (if (< i len)
          (let ((x (remainder i edge-length))
                (y (quotient i edge-length)))
            (if (= x 0.)
                (newline))
            (write (vector-ref kernel i)) (display " ")
            (loop (+ i 1)))))))

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
    (real->byte (saturate (/ value 2.5)))))

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
  (/ byte 255.))

(define (real->byte f)
  (inexact->exact
   (floor (* (saturate f) 255.))))
