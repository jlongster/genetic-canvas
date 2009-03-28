;;;; Implements a genetic algorithm for learning how to draw images.
;;;;
;;;; 3/9/2009
;;;; James Long

;; Declares

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; Required modules

(define-macro (eval-for-macros expr)
  (eval expr))

(eval-for-macros (define inline? #f))

(define-macro (require-macros name)
  `(include ,name))

(define-macro (require name)
  (if inline?
      `(include ,name)
      `(load (resource lib-path ,name))))

(require-macros "ffi/util.scm")
(require "ffi/gl/gl")
(require "ffi/gl/glu")
(require "ffi/ffi")
(require "ffi/freeimage")
(require "util/sort.scm")
(require "util/srfi-1.scm")
(require "util/srfi-13.scm")
(require "fitness-ffi")
(require "images")
(require "vectors")
(require "genetic")
(require "geometry")
(require "settings")

;; Utility

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define source-image #f)
(define population #f)

(define profile-count 0)
(define profile-average 0)

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
  (current-width width)
  (current-height height)
  (freeimage-initialize #f)
  (set! source-image (load-image (resource source-image-file)))
  (set! population (make-population 50)))

(define (shutdown-engine)
  (freeimage-deinitialize))

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

  (set! source-image (gl-scale-image-to-window (image-blur source-image)))
  (configure source-image))

(define (run-frame)
  (let ((start (real-time)))
    (glClearColor (vec3-x initial-color)
                  (vec3-y initial-color)
                  (vec3-z initial-color)
                  1.)
    (glLoadIdentity)

    ;; Run all the genotypes
    (population-run! population source-image)

    ;; Show the results
    (let ((best (population-fitness-search population >))
          (worst (population-fitness-search population <)))
      (glClear GL_COLOR_BUFFER_BIT)
      (glColor4f 1. 1. 1. .1)
;;       (image-render source-image)
      (render-genotype best))

    ;; Evolve it another time
    (set! population (population-evolve population))

    (let ((timed (- (real-time) start)))
      (set! profile-average (/ (+ (* profile-count profile-average) timed)
                               (+ profile-count 1)))
      (set! profile-count (+ profile-count 1))
      (show "time: " timed "s, ")
      (show "avg: " profile-average "s\n")
      )))
