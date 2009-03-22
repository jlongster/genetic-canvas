;;;; Implements a genetic algorithm for learning how to draw images.
;;;;
;;;; 3/9/2009
;;;; James Long

;; Declares

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; Required modules

(include "ffi/util.scm")
(load (resource lib-path "ffi/gl/gl"))
(load (resource lib-path "ffi/gl/glu"))
(load (resource lib-path "ffi/ffi"))
(load (resource lib-path "ffi/freeimage"))
(load (resource lib-path "util/sort.scm"))
(load (resource lib-path "util/srfi-1.scm"))
(load (resource lib-path "util/srfi-13.scm"))
(load (resource lib-path "settings"))
(load (resource lib-path "images"))
(load (resource lib-path "vectors"))
(load (resource lib-path "genetic"))
(load (resource lib-path "util/statprof/statprof"))

;; Utility

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define source-image #f)
(define population #f)

(define exact inexact->exact)
(define real exact->inexact)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (show . rest)
  (for-each (lambda (el)
              (display el))
            rest))

(define (gl-scale-image-to-window image)
  (let ((new-image (make-image (current-width)
                               (current-height)
                               FORMAT_RGBA)))
    (image-render image (current-width) (current-height))
    (image-read-gl-pixels! new-image)
    new-image))


;; Application
;; These are special methods which the application is aware of,
;; and are automatically called at the appropriate times.

(define (init-engine width height)
  (profile-start!)
  (current-width width)
  (current-height height)
  (freeimage-initialize #f)
  (set! source-image (load-image (resource "resources/test3.jpg")))
  (set! population (make-population 3)))

(define (shutdown-engine)
  (freeimage-deinitialize)
  (free-image source-image)
  (free-image current-image)
  (profile-stop!)
  (write-profile-report "bench")
  )

(define (init-opengl)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluOrtho2D 0.
              (real (current-width))
              (real (current-height))
              0.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glEnable GL_TEXTURE_2D)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glDisable GL_CULL_FACE)

  (set! source-image (gl-scale-image-to-window source-image))
  (optimize-settings source-image))

(define (run-frame)
  (glClearColor 1. 1. 1. 1.)
  (glLoadIdentity)
  
  ;; Run all the genotypes
  (population-run! population source-image)

  ;; Show the results
  (let ((best (population-fitness-search population >))
        (worst (population-fitness-search population <)))
    (glClear GL_COLOR_BUFFER_BIT)
    (glColor4f 1. 1. 1. .1)
    (image-render source-image)
    (render-genotype best))

  ;; Evolve it another time
  (set! population (population-evolve-three population)))
