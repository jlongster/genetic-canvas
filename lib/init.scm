
;; Include standard modules here
(include "resources.scm")

;; Load in the engine and define its interface
;;(load (resource "lib/engine"))
(include "engine.scm")

(c-define (init-opengl-c) () void "init_opengl" ""
  (init-opengl))

(c-define (init-engine-c width height)
    (unsigned-int unsigned-int) void "init_engine" ""
  (init-engine width height))

(c-define (shutdown-engine-c) () void "shutdown_engine" ""
  (shutdown-engine))

(c-define (run-frame-c) () void "run_frame" ""
  (run-frame))
