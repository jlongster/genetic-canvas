
;; --- OpenGL 2.0 shader objects -----------------------------
;; TODO: add constants here
(define glCreateShader
  (c-lambda (GLenum) GLuint "glCreateShader"))

(define glShaderSource
  (c-lambda (GLuint GLsizei nonnull-char-string-list scheme-object)
            bool #<<end-c-code
   if(sizeof(GLint) != 4 || !___S32VECTORP(___arg4))
	   // LOG
	   ___result = 0;
   else {
	   glShaderSource(___arg1,
                          ___arg2,
                          (const GLchar**)___arg3,
                          (const GLint*)___BODY(___arg4));
	   ___result = 1;
   }
end-c-code
))

(define glCompileShader
  (c-lambda (GLuint) void "glCompileShader"))

(define glDeleteShader
  (c-lambda (GLuint) void "glDeleteShader"))

(define glCreateProgram
  (c-lambda () GLuint "glCreateProgram"))

(define glAttachShader
  (c-lambda (GLuint GLuint) void "glAttachShader"))

(define glDetachShader
  (c-lambda (GLuint GLuint) void "glDetachShader"))

(define glLinkProgram
  (c-lambda (GLuint) void "glLinkProgram"))

(define glUseProgram
  (c-lambda (GLuint) void "glUseProgram"))

(define glDeleteProgram
  (c-lambda (GLuint) void "glDeleteProgram"))

(define glIsShader
  (c-lambda (GLuint) bool "glIsShader"))

(define glGetShaderiv
  (c-lambda (GLuint GLenum) GLint
            "int p; glGetShaderiv(___arg1, ___arg2, &p); return p;"))

(define glIsProgram
  (c-lambda (GLuint) bool "glIsProgram"))

(define glGetProgramiv
  (c-lambda (GLuint GLenum) GLint
            "int p; glGetProgramiv(___arg1, ___arg2, &p); return p;"))

(define glGetAttachedShaders
 (c-lambda (GLuint) scheme-object #<<end-c-code
	GLuint shaders[1024];
	GLsizei count;
	int i;
	glGetAttachedShaders(___arg1, 1024, &count, shaders);

	___SCMOBJ result = ___NUL;
	___SCMOBJ new_result;
	for(i=0; i<count; i++) {
		new_result = ___EXT(___make_pair)(___FIX(shaders[i]), result, ___STILL);
		___EXT(___release_scmobj)(result);
		result = new_result;
	}

	___EXT(___release_scmobj)(result);
	___result = result;
end-c-code))

(define glGetShaderSource
  (c-lambda (GLuint) nonnull-char-string #<<end-c-code
   GLchar source[8192];
   glGetShaderSource(___arg1, 8192, NULL, source);
   ___result = source;
end-c-code))

;; --- OpenGL 2.0 vertex shaders -----------------------------
;; --- OpenGL 2.0 fragment shaders -----------------------------
