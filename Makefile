all: Main.app/Contents/MacOS/main

entry.c: entry.scm
	gsc -debug -c entry.scm

engine.c: engine.scm
	gsc -debug -c engine.scm

link_.c: entry.c engine.c
	gsc -debug -link -o link_.c engine.c entry.c

Main.app/Contents/Info.plist: Info.plist
	mkdir -p Main.app/Contents
	cp Info.plist Main.app/Contents/Info.plist

Main.app/Contents/Resources/main.nib: main.xib
	mkdir -p Main.app/Contents/Resources
	rm -r Main.app/Contents/Resources/main.nib
	ibtool --errors --warnings --notices --output-format human-readable-text \
		--compile Main.app/Contents/Resources/main.nib main.xib

Main.app/Contents/MacOS/main: main.nib link_.c entry.c engine.c cocoa-entry.m glview.m glwindow.m Main.app/Contents/Info.plist Main.app/Contents/Resources/main.nib
	mkdir -p Main.app/Contents/MacOS
	gcc -o main link_.c entry.c engine.c cocoa-entry.m glview.m glwindow.m \
		-I/usr/local/Gambit-C/current/include \
		-framework Cocoa -framework OpenGL \
		-lgambc
	mv main Main.app/Contents/MacOS

# Below compiles the program with a C entry point
# main: main.nib main.c engine.scm 
# 	gsc -debug -link -o engine_.c engine.scm
# 	gcc -o main main.c engine*.c \
# 		-D___LIBRARY \
# 		-I/usr/local/Gambit-C/current/include \
# 		-lgambc

clean:
	rm link_.c
	rm engine.c
	rm entry.c
	rm -r Main.app
