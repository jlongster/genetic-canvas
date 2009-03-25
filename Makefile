all: main Main.app main.nib objects

main.nib: app/main.xib
	rm -rf main.nib
	ibtool --errors --warnings --notices \
		--output-format human-readable-text \
		--compile main.nib app/main.xib

app/entry.c: app/entry.scm
	gsc -c app/entry.scm

lib/init.c: lib/init.scm
	gsc -track-scheme -c lib/init.scm

app/link_.c: app/entry.c lib/init.c
	gsc -link -o app/link_.c lib/init.c app/entry.c

main: app/link_.c app/cocoa.m app/glview.m app/glwindow.m app/app.m
	gcc -o main \
		app/link_.c lib/init.c app/entry.c \
		app/cocoa.m app/glview.m \
		app/glwindow.m \
		app/app.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc \
	 	-lIL \
		-sectcreate __TEXT __info_plist app/Info.plist
# 		-lSaturn -finstrument-functions

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

lib/ffi/triangulate-ffi.o1: lib/ffi/triangulate-ffi.scm
	rm -f lib/ffi/triangulate-ffi.o1
	gsc -debug lib/ffi/triangulate-ffi.scm

lib/engine.o1: lib/engine.scm
	rm -f lib/engine.o1
	gsc -debug lib/engine.scm

lib/genetic.o1: lib/genetic.scm
	rm -f lib/genetic.o1
	gsc -debug lib/genetic.scm

lib/fitness-ffi.o1: lib/fitness-ffi.scm
	rm -f lib/fitness-ffi.o1
	gsc -debug lib/fitness-ffi.scm

lib/geometry.o1: lib/geometry.scm
	rm -f lib/geometry.o1
	gsc -debug lib/geometry.scm

lib/images.o1: lib/images.scm
	rm -f lib/images.o1
	gsc -debug -debug lib/images.scm

lib/settings.o1: lib/settings.scm
	rm -f lib/settings.o1
	gsc -debug lib/settings.scm

objects: lib/ffi/ffi.o1 \
	lib/ffi/gl/gl.o1 \
	lib/ffi/gl/glu.o1 \
	lib/ffi/freeimage.o1 \
	lib/engine.o1 \
	lib/genetic.o1 \
	lib/geometry.o1 \
	lib/images.o1 \
	lib/fitness-ffi.o1 \
	lib/settings.o1

clean-objects:
	find . -iname '*.o1' | xargs rm

clean-lib-objects:
	find . -iname '*.o1' -not -ipath '*/ffi/*' | xargs rm

## Create a Main.app so that we can also run it as a bundle app

Main.app/Contents/Resources/main.nib: main.nib
	mkdir -p Main.app/Contents/Resources
	rm -rf Main.app/Contents/Resources/main.nib
	cp -r main.nib Main.app/Contents/Resources/main.nib

Main.app/Contents/MacOS/main: main
	mkdir -p Main.app/Contents/MacOS
	cp main Main.app/Contents/MacOS

Main.app: Main.app/Contents/MacOS/main \
	  Main.app/Contents/Resources/main.nib

## Testing

test: tests/main.sw
	sweb -s tests/main.sw > tests/main.scm
	gsi -e '(include "lib/util/tests.scm")' \
		-e '(include "lib/resources.scm")' \
		tests/main.scm

# Copied from `main' test target
test-engine: tests/engine/engine.sw
	sweb -s tests/engine/engine.sw > tests/engine/engine.scm
	gsc -c tests/engine/engine.scm
	gsc -link -o tests/engine/engine_.c tests/engine/engine.c app/entry.c
	gcc -o tests/engine/engine tests/engine/engine_.c \
		tests/engine/engine.c app/entry.c \
		app/cocoa.m app/glview.m \
		app/glwindow.m \
		app/app.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc \
	 	-lIL \
		-sectcreate __TEXT __info_plist app/Info.plist
	rm -rf tests/engine/main.nib && cp -r main.nib tests/engine
	tests/engine/engine

# Copied from `main' test target
test-images: tests/images/images.sw
	sweb -s tests/images/images.sw > tests/images/images.scm
	gsc -debug -c tests/images/images.scm
	gsc -link -o tests/images/images_.c tests/images/images.c app/entry.c
	gcc -o tests/images/images tests/images/images_.c \
		tests/images/images.c app/entry.c \
		app/cocoa.m app/glview.m \
		app/glwindow.m \
		app/app.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc \
	 	-lIL \
		-sectcreate __TEXT __info_plist app/Info.plist
	rm -rf tests/images/main.nib && cp -r main.nib tests/images
	tests/images/images

test-triangulate-ffi: tests/triangulate-ffi.scm
	rm -f test/triangulate-ffi.o1
	gsc tests/triangulate-ffi.scm
	cd tests && gsi triangulate-ffi

test-triangulate: tests/triangulate.c
	cd tests && gcc -o triangulate triangulate.c
	cd tests && ./triangulate

pdf: tests/main.sw tests/engine/engine.sw tests/images/images.sw
	sweb -w tests/main.sw > tests/main.tex
	pdflatex -output-directory=tests tests/main.tex
	sweb -w tests/engine/engine.sw > tests/engine/engine.tex
	pdflatex -output-directory=tests tests/engine/engine.tex
	sweb -w tests/images/images.sw > tests/images/images.tex
	pdflatex -output-directory=tests tests/images/images.tex

## cleanup
clean: clean-objects
	rm -f app/link_.c
	rm -f lib/engine-c.c
	rm -f app/entry.c
	rm -f app/entry-end.c
	rm -f main
	rm -rf main.nib
