
#define ___VERSION 403002
#include "gambit.h"
#include "IL/il.h"
#import <Cocoa/Cocoa.h>

// From Scheme, going into Cocoa
// @args is a list of strings (as a scheme object)
int cocoa_entry(___SCMOBJ args) {
	char** argv = calloc(1028, sizeof(char*));
	int i = 0;
	
	___SCMOBJ tail = args;
	while(tail != ___NUL) {
		___SCMOBJ head = ___CAR(tail);
		___EXT(___SCMOBJ_to_CHARSTRING)(head, &argv[i], 0);
		tail = ___CDR(tail);
		i++;
	}

	int argc = i+1;

	// TODO: probably need to free() the individual strings,
	// need to look at ___SCMOBJ_to_CHARSTRING
	argv = realloc(argv, sizeof(char*)*argc);
	NSApplicationMain(argc, (const char**)argv);
	free(argv);
	return 0;
}
