
#include "engine.h"
#include "glview.h"

@implementation MainOpenGLView

-(id) initWithFrame: (NSRect)frame {
	self = [super initWithFrame:frame];
	if(self) {
		fprintf(stderr, "made a frame");
		NSTrackingArea *trackingArea =
			[[NSTrackingArea alloc] initWithRect:frame
									options: (NSTrackingMouseMoved |
											  NSTrackingActiveInKeyWindow)
									owner:self userInfo:nil];
		[self addTrackingArea: trackingArea];
	}
	return self;
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
					 selector:@selector(setNeedsDisplay:) userInfo:nil repeats:YES];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSEventTrackingRunLoopMode];
	[self setNeedsDisplay:YES];
}

-(void) drawRect: (NSRect)bounds {
	run_frame();
	[[self openGLContext] flushBuffer];
}

@end
