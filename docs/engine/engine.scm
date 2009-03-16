;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code generated by SchemeWeb from a .sw file. ;;
;; Any change will be lost !                    ;;
;; DO NOT EDIT please.                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(include "../../lib/resources.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "util/srfi-1.scm"))
(load (resource lib-path "images"))
(load (resource lib-path "vectors"))
(load (resource lib-path "genetic"))
(c-define (init-opengl-c) () void "init_opengl" ""
  (init-opengl))
(c-define (init-engine-c width height)
    (unsigned-int unsigned-int) void "init_engine" ""
  (init-engine width height))
(c-define (shutdown-engine-c) () void "shutdown_engine" ""
  (shutdown-engine))
(c-define (run-frame-c) () void "run_frame" ""
  (run-frame))
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

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glDisable GL_CULL_FACE))
(define (shutdown-engine)
  (void))
(define (run-frame)
  (glClearColor 0. 1. 0. 1.)
  (glClear GL_COLOR_BUFFER_BIT)

  (glBegin GL_POLYGON)
  (glColor4f 0. 0. 1. .5)
  (glVertex2f 0. 0.)
  (glVertex2f 0. 100.)
  (glVertex2f 220. 30.)
  (glVertex2f 150. 60.)
  (glVertex2f 40. 30.)
  (glEnd))
(define (render-polygon points)
  (void))