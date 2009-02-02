
(load (resource object-path "ffi/gl/gl"))
(load (resource object-path "ffi/gl/glu"))
(load (resource object-path "ffi/ffi"))
(load (resource object-path "ffi/freeimage"))

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (init-opengl width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.
              (current-width)
              (current-height)
              0.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glEnable GL_TEXTURE_2D)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (let ((img (freeimage-load-jpg
              (resource "resources/monalisa.jpg")))
        (tex (let ((buf (alloc-uint 1)))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGB
                  (freeimage-width img)
                  (freeimage-height img)
                  0
                  GL_RGB
                  GL_UNSIGNED_BYTE
                  (u8*->void* (freeimage-bytes img)))
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
                     GL_REPEAT)))

(define (init-engine)
  'void)

(define (run-frame)
  (glLoadIdentity)
  (glClearColor 0.2 0.3 0.4 1.0)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glColor3f 1.0 0.9 0.8)
  (glBegin GL_QUADS)
  (begin
    (glTexCoord2d 1. 1.)
    (glVertex2f 0. 0.))
  (begin
    (glTexCoord2d 1. 0.)
    (glVertex2f 0. (current-height)))
  (begin
    (glTexCoord2d 0. 0.)
    (glVertex2f (current-width)
                (current-height)))
  (begin
    (glTexCoord2d 0. 1.)
    (glVertex2f (current-width) 0.))
  (glEnd))
