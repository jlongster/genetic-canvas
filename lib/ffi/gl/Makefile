
AUTOFFI="../../../autoffi/autoffi"

all: gl.o1 glu.o1

types.scm: gl.h
	($(AUTOFFI) gl.h | grep 'c-define-type') 2>/dev/null >types.scm

gl.scm: types.scm gl.h
	{ \
		echo '(c-declare "#include \"gl/gl.h\"")'; \
		echo '(include "types.scm")'; \
		($(AUTOFFI) gl.h | grep -v 'c-define-type') 2>/dev/null; \
	}  >gl.scm

glu.scm: types.scm glu.h
	{ \
		echo '(c-declare "#include \"gl/glu.h\"")'; \
		echo '(include "types.scm")'; \
		$(AUTOFFI) glu.h 2>/dev/null; \
	} >glu.scm

gl.o1: gl.scm
	rm -f gl.o1
	gsc -ld-options "-framework OpenGL" gl.scm

glu.o1: glu.scm
	rm -f glu.o1
	gsc glu.scm

glext.o1: glext.scm
	rm -f glext.o1
	gsc -ld-options "-framework OpenGL" glext.scm
