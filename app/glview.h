
#import <Cocoa/Cocoa.h>

@interface MainOpenGLView : NSOpenGLView {
	float rot;
	NSTimer* timer;
}
-(void) awakeFromNib;
-(void) prepareOpenGL;
-(BOOL) acceptsFirstResponder;
-(BOOL) becomeFirstResponder;
-(void) mouseMoved:(NSEvent *)theEvent;
-(id) initWithFrame:(NSRect)frame;
-(void) drawRect: (NSRect)bounds;
@end
