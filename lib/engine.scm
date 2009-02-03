
(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "ffi/util.scm")

(load (resource object-path "ffi/gl/gl"))
(load (resource object-path "ffi/gl/glu"))
(load (resource object-path "ffi/ffi"))
(load (resource object-path "ffi/freeimage"))

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define mona-lisa-tex #f)
(define mona-lisa-tex2 #f)
(define texture-buffer #f)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (init-engine)
  (freeimage-initialize #f))

(define (shutdown-engine)
  (freeimage-deinitialize))

(define (load-jpg filename)
  (let ((img (freeimage-load-jpg filename))
        (tex (with-alloc (buf (alloc-uint 1))
               (glGenTextures 1 buf)
               (uint*-ref buf 0))))
    (glBindTexture GL_TEXTURE_2D tex)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (with-alloc (bytes (freeimage-bytes img))
                (glTexImage2D GL_TEXTURE_2D
                              0
                              GL_RGB
                              (freeimage-width img)
                              (freeimage-height img)
                              0
                              GL_RGB
                              GL_UNSIGNED_BYTE
                              (u8*->void* (freeimage-bytes img))))    
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
    tex))

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
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

  (set! mona-lisa-tex (load-jpg (resource "resources/monalisa.jpg")))
  (set! mona-lisa-tex2 (load-jpg (resource "resources/monalisa2.jpg")))
  (set! texture-buffer
        (alloc-u8
         (inexact->exact (* (current-width) (current-height) 4)))))

(define (run-frame)
  (glLoadIdentity)
  (glClearColor 1. 1. 1. 1.)
  (glClear GL_COLOR_BUFFER_BIT)

  ;; Draw mona lisa
  (glBindTexture GL_TEXTURE_2D mona-lisa-tex2)
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
  (glEnd)
  (glBindTexture GL_TEXTURE_2D 0)

  ;; Draw a polygon over her face
  (glBegin GL_TRIANGLES)
  (let ((state (random-source-state-ref default-random-source)))
    (let loop ((i 0))
      (glColor4f (* (random-real) 1.0)
                 (* (random-real) 1.0)
                 (* (random-real) 1.0)
                 0.1)
      (glVertex2f (* (random-real) (current-width))
                  (* (random-real) (current-height)))
      (glVertex2f (* (random-real) (current-width))
                  (* (random-real) (current-height)))
      (glVertex2f (* (random-real) (current-width))
                  (* (random-real) (current-height)))
      (if (< i 100)
          (loop (+ i 1))))
;;     (random-source-state-set! default-random-source state)
    )
  (glEnd)
  (glColor4f 0.0 0.0 0.0 1.0)

  ;; Draw mona lisa
  (glEnable GL_COLOR_LOGIC_OP)
  (glLogicOp GL_XOR)
  (glBindTexture GL_TEXTURE_2D mona-lisa-tex)
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
  (glEnd)
  (glBindTexture GL_TEXTURE_2D 0)
  (glDisable GL_COLOR_LOGIC_OP)

  (let ((widthi (inexact->exact (current-width)))
        (heighti (inexact->exact (current-height))))
    (glReadPixels 0 0
                  widthi heighti
                  GL_RGBA GL_UNSIGNED_BYTE
                  (u8*->void* texture-buffer))
    (display (sum-u8* texture-buffer (* widthi heighti 4)))
    (newline)))

