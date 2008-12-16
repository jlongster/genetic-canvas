
(include "opengl-ffi/opengl.scm")

(define rot 0.0)

(c-define (run-frame) () void "run_frame" ""
  (set! rot (+ rot 0.1))
  (glRotatef rot 0.0 1.0 0.0)
  (glClearColor 0.2 0.3 0.4 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3f 1.0 0.9 0.8)
  (glBegin GL_TRIANGLES)
  (glVertex3f 0.0 0.6 0.0)
  (glVertex3f -0.2 -0.3 0.0)
  (glVertex3f 0.2 -0.3 0.0)
  (glEnd))

