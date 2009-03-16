
\documentclass{article}
\begin{document}

\title{Images}
\author{James Long}
\date{March 12, 2009}
\maketitle

Provides functionality for loading images, processing them and
uploading them to OpenGL.

(include "../../lib/resources.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "ffi/ffi"))
(load (resource lib-path "ffi/freeimage"))
(load (resource lib-path "util/srfi-13"))
(load (resource lib-path "images"))

\section{Entry Points}

These define all of the entry points into scheme world used by Cocoa.

(c-define (init-opengl-c) () void "init_opengl" ""
  (init-opengl))

(c-define (init-engine-c width height)
    (unsigned-int unsigned-int) void "init_engine" ""
  (init-engine width height))

(c-define (shutdown-engine-c) () void "shutdown_engine" ""
  (shutdown-engine))

(c-define (run-frame-c) () void "run_frame" ""
  (run-frame))

\section{Engine}

Now we define our simple engine.

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))
(define texture #f)

(define (init-engine width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))
  (freeimage-initialize #f)
  
  (set! texture (make-image "resources/monalisa_small.jpg"))
;;   (set! texture (make-image "docs/images/test.jpg"))
  (image-bytes-set! texture
                    (edge-filter
                     (gaussian-blur-filter
                      (rgb->greyscale (image-bytes texture)
                                      (* (image-width texture)
                                         (image-height texture)))
                      (image-width texture)
                      (image-height texture))
                     (image-width texture)
                     (image-height texture)))
  (make-edges-white! (image-bytes texture)
                     (image-width texture)
                     (image-height texture))
  (image-format-set! texture FORMAT_LUMINANCE))

(define (init-opengl)
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
  
  (image-opengl-upload! texture))

(define (shutdown-engine)
  (freeimage-deinitialize))

(define (run-frame)
  (glClearColor 0. 1. 0. 1.)
  (glClear GL_COLOR_BUFFER_BIT)

  (glBindTexture GL_TEXTURE_2D (image-gl-texture-id texture))
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
  
\section{Filtering}

\subsection{(rgb->greyscale bytes length)}

Convertes RGB data to greyscale. bytes should be a u8 buffer allocated
from C (usually the result of one of the image loading procedures).


