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
(load (resource lib-path "genetic"))
;; (load (resource lib-path "util/statprof/statprof"))

;; Utility

(define current-width (make-parameter 0))
(define current-height (make-parameter 0))

(define source-image #f)
(define source-image-edges #f)
(define source-image-sized #f)
(define texture-buffer #f)

(define population #f)

(define (display-gl-error)
  (display (gluErrorString (glGetError))))

(define (show . rest)
  (for-each (lambda (el)
              (display el))
            rest))


;; Application
;; These are special methods which the application is aware of,
;; and are automatically called at the appropriate times.

(define (init-engine width height)
;;   (profile-start!)
  (current-width (exact->inexact width))
  (current-height (exact->inexact height))

  (freeimage-initialize #f)
  (set! source-image (load-image (resource "resources/test1.jpg")))
;;   (set! source-image-edges (load-image (resource "resources/monalisa.jpg")))

;;   (image-bytes-set! source-image-edges
;;                     (edge-filter
;;                      (gaussian-blur-filter
;;                       (rgb->greyscale (image-bytes source-image-edges)
;;                                       (* (image-width source-image-edges)
;;                                          (image-height source-image-edges)))
;;                       (image-width source-image-edges)
;;                       (image-height source-image-edges))
;;                      (image-width source-image-edges)
;;                      (image-height source-image-edges)))
;;   (image-format-set! source-image-edges FORMAT_LUMINANCE)
  
  (set! population (make-population 2)))

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
                                                    4))))

  (let ((tmp-buffer (alloc-u8 (inexact->exact (* (current-width)
                                                 (current-height)
                                                 4)))))
    (image-render source-image)
    (glReadPixels 0 0
                  (inexact->exact (current-width))
                  (inexact->exact (current-height))
                  GL_RGBA GL_UNSIGNED_BYTE
                  (u8*->void* tmp-buffer))
    (set! source-image-sized (make-image (inexact->exact (current-width))
                                         (inexact->exact (current-height))
                                         tmp-buffer
                                         FORMAT_RGBA
                                         #f)))
  (analyze-source source-image-sized)

  (show "min-red: " min-red) (newline)
  (show "max-red: " max-red) (newline)
  (show "min-green: " min-green) (newline)
  (show "max-green: " max-green) (newline)
  (show "min-blue: " min-blue) (newline)
  (show "max-blue: " max-blue) (newline))

(define (run-frame)
  (glClearColor 0. 0. 0. 1.)
  (glLoadIdentity)
  
  ;; Run all the genotypes
  (fold (lambda (el acc)
          (genotype-fitness-set! el (run-genotype el)))
        #f
        population)

  (let ((best (population-fitness-search population <))
        (worst (population-fitness-search population >)))
    (glClear GL_COLOR_BUFFER_BIT)
    (render-genotype best)
    (glColor4f 1. 1. 1. .1)
    (image-render source-image-sized)
    (show "overall fitness: "
          (overall-fitness (genotype-fitness best)) " "
          (overall-fitness (genotype-fitness worst)) "\n")
    (population-normalize population
                          (genotype-fitness best)
                          (genotype-fitness worst))
    (population-evolve-two! population)))
