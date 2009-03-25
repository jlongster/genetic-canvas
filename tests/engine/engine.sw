
\documentclass{article}
\begin{document}

\title{Basic Engine}
\author{James Long}
\date{March 12, 2009}
\maketitle

This is a basic testbed for rendering things with OpenGL for visual
approval.  It replaces the engine defined in lib/engine.scm.

Load our required modules.

(include "../../lib/resources.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "util/srfi-1.scm"))
(load (resource lib-path "images"))
(load (resource lib-path "vectors"))
(load (resource lib-path "genetic"))
(load (resource lib-path "geometry"))

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

Now we define our simple engine.  This is a good example of how to use
this simple framework.

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define polys #f)

(define (init-engine width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))
  
  (set! polys (unfold (lambda (i) (>= i 10))
                      (lambda (i) (random-polygon))
                      (lambda (i) (+ i 1))
                      0)))

(define (init-opengl)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.
              (current-width)
              (current-height)
              0.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glDisable GL_CULL_FACE))

(define (shutdown-engine)
  (void))

(define (run-frame)
  (glClearColor 1. 1. 1. 1.)
  (glClear GL_COLOR_BUFFER_BIT)

  (for-each (lambda (el)
              (render-polygon el))
            polys))

\section{Polygons}

A polygon has the following type:

(define-structure polygon
  points
  red
  green
  blue
  alpha)

\subsection{(render-polygon poly)}

This will render a polygon using the number of points and color
defined in `poly'.  No triangulation method is applied to the polygon;
its points are passed straight into OpenGL.

\end{document}
