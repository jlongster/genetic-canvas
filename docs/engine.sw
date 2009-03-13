
\documentclass{article}
\begin{document}

\title{Basic Engine}
\author{James Long}
\date{March 12, 2009}
\maketitle

This is a basic testbed for rendering things with OpenGL for visual
approval.  It replaces the engine defined in lib/engine.scm.

Load our required modules.

(include "../lib/resources.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "util/srfi-1.scm"))
(load (resource lib-path "vectors"))
(load (resource lib-path "genotypes"))

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

(define gt #f)

(define (init-engine width height)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))
  
  (set! gt (random-genotype)))

(define (init-opengl)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.
              (current-width)
              (current-height)
              0.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glDisable GL_CULL_FACE))

(define (shutdown-engine)
  (void))

(define (run-frame)
  (glClearColor 0. 1. 0. 1.)
  (glClear GL_COLOR_BUFFER_BIT)

  (render-genotype gt))

\section{Polygons}

(define (render-polygon points)
  (void))

\end{document}
