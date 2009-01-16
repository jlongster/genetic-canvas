
#include "lib/init.h"
#include "glview.h"

@implementation MainOpenGLView

-(id) initWithFrame: (NSRect)frame {
	self = [super initWithFrame:frame];
	if(self) {
		NSTrackingArea *trackingArea =
			[[NSTrackingArea alloc] initWithRect:frame
									options: (NSTrackingMouseMoved |
											  NSTrackingActiveInKeyWindow)
									owner:self userInfo:nil];
		[self addTrackingArea: trackingArea];
	}
	return self;
}

-(void) prepareOpenGL {
	NSRect bounds = [self bounds];
	init_opengl((unsigned int)NSWidth(bounds), (unsigned int)NSHeight(bounds));
}

-(BOOL) acceptsFirstResponder {
	return YES;
}

- (BOOL)becomeFirstResponder {
  return  YES;
}

-(void) mouseMoved: (NSEvent *)event {
	fprintf(stderr, "Hello");
	[super mouseMoved:event];
}

-(void) awakeFromNib {
	rot = 0.0f;
	timer = [NSTimer timerWithTimeInterval:(1.0f/60.0f) target:self
					 selector:@selector(drawRect:) userInfo:nil repeats:YES];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSEventTrackingRunLoopMode];
	[self setNeedsDisplay:YES];
	init_engine();
}

-(void) drawRect: (NSRect)bounds {
	run_frame();
	[[self openGLContext] flushBuffer];
	fflush(stdout);
}

@end
