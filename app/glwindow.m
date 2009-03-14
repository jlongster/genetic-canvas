
#include "glwindow.h"

@implementation MainOpenGLWindow
-(id) initWithContentRect:(NSRect)contentRect
				styleMask:(unsigned int)aStyle
				backing:(NSBackingStoreType)bufferingType
				defer:(BOOL)flag {
	NSWindow *window = [super initWithContentRect:contentRect
							  styleMask:NSBorderlessWindowMask | NSTitledWindowMask 
							  backing:bufferingType
							  defer:flag];
	//[window setLevel:NSFloatingWindowLevel];
	//[window setOpaque:NO];
	//[window setAlphaValue:0.6];
	//[window setAcceptsMouseMovedEvents:YES];
	return window;
}
@end
