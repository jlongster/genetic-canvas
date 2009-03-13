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
(load (resource lib-path "images"))
(load (resource lib-path "vectors"))
(load (resource lib-path "genotypes"))
;; (load (resource lib-path "util/statprof/statprof"))

;; Utility

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define source-image #f)
(define texture-buffer #f)

(define population #f)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))


;; Application
;; These are special methods which the application is aware of,
;; and are automatically called at the appropriate times.

(define (init-engine width height)
;;   (profile-start!)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))

  (set! source-image (make-image (resource "resources/monalisa.jpg")))
  (set! population (make-population 2))
  (freeimage-initialize #f))

(define (shutdown-engine)
  (freeimage-deinitialize)
;;   (profile-stop!)
;;   (write-profile-report "artbot.txt")
  )

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
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

  (image-opengl-upload! source-image)
  
  (set! texture-buffer (alloc-u8 (inexact->exact (* (current-width)
                                                    (current-height)
                                                    4)))))

(define (run-frame)
  (glClearColor 1. 1. 1. 1.)
  (glLoadIdentity)

  ;; Run all the genotypes
  (let loop ((pop population)
             (best 0)
             (worst 0))
    (if (null? pop)
        (population-normalize population best worst)
        (let* ((gt (car pop))
               (fitness (run-genotype gt)))
          (genotype-fitness-set! gt fitness)
          (loop (cdr pop)
                (min best fitness)
                (max worst fitness)))))

  (glClear GL_COLOR_BUFFER_BIT)
  (render-genotype (car population))
  
  (population-evolve! population))
