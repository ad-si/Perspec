// Receives the files macOS opens with the app,
// e.g. when they are dropped onto its icon in the Dock.
//
// GLFW's application delegate doesn't handle them,
// so the method is added to its class at runtime.
// macOS can deliver the files of a single drop in several batches,
// and the first ones already while GLFW creates the first window.
// Therefore the batches are buffered until a callback is set.

#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>
#include <stdbool.h>
#include <stdlib.h>

typedef void (*OpenFilesCallback)(int count, const char **paths);

static OpenFilesCallback openFilesCallback = NULL;
static NSMutableArray<NSArray<NSString *> *> *pendingBatches = nil;

static void deliverBatch(NSArray<NSString *> *paths) {
  @autoreleasepool {
    const char **cPaths = malloc(paths.count * sizeof(char *));
    for (NSUInteger index = 0; index < paths.count; index++) {
      cPaths[index] = paths[index].fileSystemRepresentation;
    }
    openFilesCallback((int)paths.count, cPaths);
    free(cPaths);
  }
}

// Implementation of `-[NSApplicationDelegate application:openFiles:]`
static void applicationOpenFiles(id self, SEL command,
                                 NSApplication *application,
                                 NSArray<NSString *> *filenames) {
  if (openFilesCallback != NULL) {
    deliverBatch(filenames);
  } else {
    [pendingBatches addObject:[[filenames copy] autorelease]];
  }
  [application replyToOpenOrPrint:NSApplicationDelegateReplySuccess];
}

// Must be called before GLFW is initialized,
// as AppKit checks the abilities of the delegate when it's set.
// Returns false if GLFW's delegate class doesn't exist.
bool perspec_install_open_files_handler(void) {
  Class delegateClass = objc_getClass("GLFWApplicationDelegate");
  if (delegateClass == Nil) {
    return false;
  }
  if (pendingBatches == nil) {
    pendingBatches = [[NSMutableArray alloc] init];
  }
  class_addMethod(delegateClass, @selector(application:openFiles:),
                  (IMP)applicationOpenFiles, "v@:@@");
  return true;
}

// Delivers the buffered batches immediately and all further ones directly
void perspec_set_open_files_callback(OpenFilesCallback callback) {
  openFilesCallback = callback;
  for (NSArray<NSString *> *batch in pendingBatches) {
    deliverBatch(batch);
  }
  [pendingBatches removeAllObjects];
}
