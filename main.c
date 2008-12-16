
//#import <Cocoa/Cocoa.h>
#define ___VERSION 403002
#include "gambit.h"
#include "engine.h"
#include "stdio.h"

#define LINKER ____20_engine__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;

int main(int argc, char* argv[]) {
	___setup_params_reset (&setup_params);
	setup_params.version = ___VERSION;
	setup_params.linker = LINKER;

	___setup(&setup_params);
	//int ret = NSApplicationMain(argc, argv);
	run_frame();
	fflush(stdout);
	___cleanup();
	return 0;
}
