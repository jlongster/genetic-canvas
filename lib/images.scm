
(include "ffi/util.scm")

;; Images

(define-type image
  id: EF8C0406-A5C4-48A6-BC1B-B615E1FEEFA6
  constructor: really-make-image
  width
  height
  rgb-bytes
  gl-texture-id)

(define (make-image filename)
  (if (not (or (not (eq? (string-suffix-length filename ".jpg") 0))
               (not (eq? (string-suffix-length filename ".jpeg") 0))))
      (raise '("Image type not supported", filename)))  
  (let ((img (freeimage-load-jpg filename)))
    (really-make-image (freeimage-width img)
                       (freeimage-height img)
                       (freeimage-bytes img)
                       #f)))

(define (image-opengl-upload! image)
  (let ((tex (with-alloc (buf (alloc-uint 1))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGB
                  (image-width image)
                  (image-height image)
                  0
                  GL_RGB
                  GL_UNSIGNED_BYTE
                  (u8*->void* (image-rgb-bytes image)))
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
           (data (image-rgb-bytes image)))
      (make-vec3 (char->integer (u8*-ref data byte-offset))
                 (char->integer (u8*-ref data (+ byte-offset 1)))
                 (char->integer (u8*-ref data (+ byte-offset 2)))))))
