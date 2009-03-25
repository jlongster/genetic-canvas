
\documentclass{article}
\begin{document}

\title{Images}
\author{James Long}
\date{March 12, 2009}
\maketitle

Provides functionality for loading images, uploading them to OpenGL,
and applying various filters on them.

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

Now we define our simple engine.  We're trying to test our image
filters, so lets show a colored image blurred with a gaussian filter.

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))
(define texture #f)

(define (init-engine width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))
  (freeimage-initialize #f)
  
  (set! texture
        (image-blur
         (load-image (resource "tests/images/test.jpg")))))

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
  (image-render texture (current-width) (current-height)))

\end{document}
