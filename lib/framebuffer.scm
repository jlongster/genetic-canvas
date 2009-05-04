
(include "ffi/util.scm")

(define (alloc-framebuffer)
  (with-alloc (fbo (alloc-uint 1))
    (glGenFramebuffersEXT 1 fbo)
    (uint*-ref fbo 0)))

(define (alloc-renderbuffer)
  (with-alloc (render (alloc-uint 1))
    (glGenRenderbuffersEXT 1 render)
    (uint*-ref render 0)))

(define (alloc-framebuffer-image width height)
  (let ((img (alloc-opengl-image)))
    (glBindTexture GL_TEXTURE_2D img)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (glTexImage2D GL_TEXTURE_2D
                  0
                  GL_RGB8
                  width
                  height
                  0
                  GL_RGBA
                  GL_UNSIGNED_BYTE
                  NULL)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MIN_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_MAG_FILTER
                     GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_S
                     GL_CLAMP_TO_EDGE)
    (glTexParameteri GL_TEXTURE_2D
                     GL_TEXTURE_WRAP_T
                     GL_CLAMP_TO_EDGE)
    (glBindTexture GL_TEXTURE_2D 0)
    (really-make-image width height NULL GL_RGBA img)))

(define (framebuffer-attach point buffer)
  (glFramebufferRenderbufferEXT GL_FRAMEBUFFER_EXT
                                point
                                GL_RENDERBUFFER_TARGET
                                buffer))

(define (framebuffer-attach-image point img)
  (glFramebufferTexture2DEXT GL_FRAMEBUFFER_EXT
                             point
                             GL_TEXTURE_2D
                             img
                             0))

(define (framebuffer-select buffer)
  (glBindFramebufferEXT GL_FRAMEBUFFER_EXT buffer))

(define (framebuffer-check-status)
  (if (not (eq? (glCheckFramebufferStatusEXT GL_FRAMEBUFFER_EXT)
                GL_FRAMEBUFFER_COMPLETE_EXT))
      (error "Framebuffer is not complete")))
