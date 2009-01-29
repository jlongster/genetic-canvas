
(c-declare "#include \"gl/gl.h\"")
(c-declare "#include \"string.h\"")

(include "ffi/gl/types.scm")
(load (resource object-path "ffi/gl/gl"))
(load (resource object-path "ffi/gl/glu"))
(load (resource object-path "obj-loader"))

(define medium #f)

(define create-program
 (c-lambda () GLuint "___result = glCreateProgram();"))

(define link-program
 (c-lambda (GLuint) void "glLinkProgram(___arg1);"))

(define use-program
 (c-lambda (GLuint) void "glUseProgram(___arg1);"))

(define GL_VERTEX_SHADER ((c-lambda () GLuint "___result = GL_VERTEX_SHADER;")))
(define GL_FRAGMENT_SHADER ((c-lambda () GLuint "___result = GL_FRAGMENT_SHADER;")))

(define really-create-shader
  (c-lambda (GLuint GLuint nonnull-char-string-list nonnull-char-string) void #<<end-code
   GLuint program = ___arg1;
   GLuint type = ___arg2;
   char** src = ___arg3;
   char* file = ___arg4;

   GLuint shader = glCreateShader(type);

   int line_count = 0;
   while(src[line_count]) line_count++;

   glShaderSource(shader, line_count, (const char**)src, NULL);
   glCompileShader(shader);

   int status;
   glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
   if(!status) {
	   printf("ERROR: COMPILING '%s' FAILED!\n", file);

	   char log[1024];
	   glGetShaderInfoLog(shader, 1024, NULL, log);
	   printf(log);
   }
   else {
	   glAttachShader(program, shader);
   }
end-code
))

(define (create-shader program type file)
  (call-with-input-file (resource resource-path file)
    (lambda (p)
      (really-create-shader program type (read-all p read-line) file))))

(define (init-opengl width height)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 60.0
                  (/ (exact->inexact width)
                     (exact->inexact height))
                  1.0
                  10000.0)
  (gluLookAt 0.0 0.0 0.0
             0.0 0.0 1.0
             0.0 1.0 0.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (let ((program (create-program)))
    (create-shader program GL_VERTEX_SHADER "shader.vtx")
    (link-program program)
    (use-program program)))

(define (init-engine)
  (set! medium (obj-load (resource "resources/medium.obj"))))

(define x 0.0)

(define (run-frame)
  (set! x (+ x 1.5))
  (glLoadIdentity)
  (glTranslatef 0.0 0.0 100.0)
  (glRotatef x 0.0 0.0 1.0)
  (glRotatef x 1.0 0.0 0.0)
  (glClearColor 0.2 0.3 0.4 1.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3f 1.0 0.9 0.8)
  (glBegin GL_TRIANGLES)
  (for-each
   (lambda (triangle)
     (glVertex3f (vector3d-x (triangle-v1 triangle))
                 (vector3d-y (triangle-v1 triangle))
                 (vector3d-z (triangle-v1 triangle)))
     (glVertex3f (vector3d-x (triangle-v2 triangle))
                 (vector3d-y (triangle-v2 triangle))
                 (vector3d-z (triangle-v2 triangle)))
     (glVertex3f (vector3d-x (triangle-v3 triangle))
                 (vector3d-y (triangle-v3 triangle))
                 (vector3d-z (triangle-v3 triangle))))
   (obj-data-triangles medium))
  (glEnd))

