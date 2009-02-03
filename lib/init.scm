
;; Include standard modules here
(include "resources.scm")

;; Load in the engine and define its interface
(load (resource "lib/engine"))

(c-define (init-opengl-c width height) (unsigned-int unsigned-int) void "init_opengl" ""
  (init-opengl width height))

(c-define (init-engine-c) () void "init_engine" ""
  (init-engine))

(c-define (shutdown-engine-c) () void "shutdown_engine" ""
  (shutdown-engine))

(c-define (run-frame-c) () void "run_frame" ""
  (run-frame))
