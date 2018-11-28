/*
 *   Fake a mouse click at position x,y
 *
 *     Usage: getMouseCoords
 *
 *   Get the coordinates of the mouse.
 *
 *   Got original code from Stackoverflow:
 *     https://stackoverflow.com/questions/3585871/how-can-i-get-the-current-mouse-pointer-position-co-ordinates-in-x
 *
 *   Any comments are mine in my attempt to grok the code.
 *
 */

#include <X11/Xlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <malloc.h>

static int _XlibErrorHandler(Display *display, XErrorEvent *event) {
   fprintf(stderr, "An error occured detecting the mouse position\n");
   return True;
}

int main(void) {
    int ii;
    int number_of_screens;
    Bool result;
    Window *root_windows;
    Window window_returned;
    int root_x, root_y;
    int win_x, win_y;
    unsigned int mask_return;

    Display *display = XOpenDisplay(NULL);
    assert(display);
    XSetErrorHandler(_XlibErrorHandler);
    number_of_screens = XScreenCount(display);
    fprintf(stderr, "There are %d screens available in this X session\n", number_of_screens);
    root_windows = (Window *)malloc(sizeof(Window) * number_of_screens);
    for (ii = 0; ii < number_of_screens; ii++) {
        root_windows[ii] = XRootWindow(display, ii);
    }
    for (ii = 0; ii < number_of_screens; ii++) {
       result = XQueryPointer(display,
                              root_windows[ii],
                              &window_returned,
                              &window_returned,
                              &root_x,
                              &root_y,
                              &win_x,
                              &win_y,
                              &mask_return);
       if (result == True) break;
    }
    if (result != True) {
        fprintf(stderr, "No mouse found.\n");
        return -1;
    }
    printf("Mouse is at (%d,%d)\n", root_x, root_y);

    free(root_windows);
    XCloseDisplay(display);

    return 0;
}
