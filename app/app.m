
#include "lib/init.h"
#include "app.h"

@implementation AppDelegate

-(void) applicationWillTerminate:(NSNotification *)n {
	shutdown_engine();
}

@end
