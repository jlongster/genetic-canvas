REVISION_ID = r1

GLUT_FILES = opengl.scm opengl-header.scm glu.scm glu-header.scm glut.scm glut-header.scm

tarball:
	tar cvzf Opengl-ffi-$(REVISION_ID).tgz $(foreach file, $(GLUT_FILES) makefile, ../opengl-ffi/$(file))

clean:
	rm -f *.tgz

