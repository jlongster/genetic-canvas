all: main Main.app main.nib

main.nib: app/main.xib
	rm -rf main.nib
	ibtool --errors --warnings --notices \
		--output-format human-readable-text \
		--compile main.nib app/main.xib

app/entry.c: app/entry.scm
	gsc -c app/entry.scm

lib/init.c: lib/init.scm lib/resources.scm
	gsc -c lib/init.scm

app/link_.c: app/entry.c lib/init.c
	gsc -link -o app/link_.c lib/init.c app/entry.c

main: app/link_.c app/cocoa.m app/glview.m app/glwindow.m app/app.m
	gcc -o main app/link_.c lib/init.c app/entry.c \
		app/cocoa.m app/glview.m \
		app/glwindow.m \
		app/app.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc \
	 	-lIL \
		-sectcreate __TEXT __info_plist app/Info.plist

lib/ffi/ffi.o1: lib/ffi/ffi.scm
	rm -f lib/ffi/ffi.o1
	gsc lib/ffi/ffi.scm

lib/ffi/gl/gl.o1: lib/ffi/gl/gl.scm
	rm -f lib/ffi/gl/gl.o1
	gsc -cc-options '-framework OpenGL' lib/ffi/gl/gl.scm

lib/ffi/gl/glu.o1: lib/ffi/gl/glu.scm
	rm -f lib/ffi/gl/glu.o1
	gsc -cc-options '-framework OpenGL' lib/ffi/gl/glu.scm

lib/ffi/freeimage.o1: lib/ffi/freeimage.scm
	rm -f lib/ffi/freeimage.o1
	gsc -ld-options "-L/opt/local/lib -lfreeimage" \
		lib/ffi/freeimage.scm

lib/engine.o1: lib/engine.scm lib/genotypes.scm lib/genetic-operators.scm
	rm -f lib/engine.o1
	gsc lib/engine.scm
	rm -f lib/genetic.o1
	gsc lib/genetic.scm

lib/obj-loader.o1: lib/obj-loader.scm
	rm -f lib/obj-loader.o1
	gsc lib/obj-loader.scm

objects: lib/engine.o1 \
	lib/opengl-ffi/opengl.o1 \
	lib/opengl-ffi/glu.o1 \
	lib/obj-loader.o1

clean-objects:
	find . -iname '*.o1' -not -iname 'opengl*' \
			     -not -iname 'glu*' | xargs rm

## For compiling as a bundled app
# Main.app/Contents/Info.plist: Info.plist
# 	mkdir -p Main.app/Contents
# 	cp Info.plist Main.app/Contents/Info.plist

Main.app/Contents/Resources/main.nib: main.nib
	mkdir -p Main.app/Contents/Resources
	rm -rf Main.app/Contents/Resources/main.nib
	cp -r main.nib Main.app/Contents/Resources/main.nib

Main.app/Contents/MacOS/main: main
	mkdir -p Main.app/Contents/MacOS
	cp main Main.app/Contents/MacOS

Main.app: Main.app/Contents/MacOS/main \
	  Main.app/Contents/Resources/main.nib

## testing
test: docs/main.sw
	sweb -s docs/main.sw > docs/main.scm
	gsi -e '(include "lib/util/tests.scm")' \
		-e '(include "lib/resources.scm")' \
		docs/main.scm

test-engine: docs/engine.sw
	sweb -s docs/engine.sw > docs/engine.scm
	gsc -c docs/engine.scm
	gsc -link -o docs/engine_.c docs/engine.c app/entry.c
	gcc -o docs/main docs/engine_.c docs/engine.c app/entry.c \
		app/cocoa.m app/glview.m \
		app/glwindow.m \
		app/app.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc \
	 	-lIL \
		-sectcreate __TEXT __info_plist docs/Info.plist
	docs/main

pdf: docs/main.sw docs/engine.sw
	sweb -w docs/main.sw > docs/main.tex
	pdflatex -output-directory=docs docs/main.tex
	sweb -w docs/engine.sw > docs/engine.tex
	pdflatex -output-directory=docs docs/engine.tex

## cleanup
clean: clean-objects
	rm -f app/link_.c
	rm -f lib/engine-c.c
	rm -f app/entry.c
	rm -f app/entry-end.c
	rm -f main
	rm -rf Main.app
	rm -rf main.nib
