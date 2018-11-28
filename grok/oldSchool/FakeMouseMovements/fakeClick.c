/*
 *   Fake a mouse click at position x,y
 *
 *     Usage: fakeClick 1000 100
 *
 *   Positions cursor x units down & y units to right from 
 *   the upper left hand courner of screen.  Then performs
 *   a left mouse button click.  The latter does not work.
 *
 *   Got original code from Stackoverflow:
 *     https://stackoverflow.com/questions/20595716/control-mouse-by-writing-to-dev-input-mice/20772019#20772019
 *
 *   Any comments are mine in my attempt to grok the code.
 *
 */
#include <X11/Xlib.h>
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void mouseClick(Display *display, int button) {

    XEvent event;

    memset(&event, 0x00, sizeof(event));

    // Setup event to be a button down event
    event.type = ButtonPress;
    event.xbutton.button = button;
    event.xbutton.same_screen = True;

    // Not sure what these next 3 statements are doing.
    // Possibly drilling up through the Z-order?
    XQueryPointer(display,
                  RootWindow(display, DefaultScreen(display)),
                  &event.xbutton.root,
                  &event.xbutton.window,
                  &event.xbutton.x_root,
                  &event.xbutton.y_root,
                  &event.xbutton.x,
                  &event.xbutton.y,
                  &event.xbutton.state);

    event.xbutton.subwindow = event.xbutton.window;

    while(event.xbutton.subwindow) {
        event.xbutton.window = event.xbutton.subwindow;
        XQueryPointer(display,
                      event.xbutton.window, 
                      &event.xbutton.root,
                      &event.xbutton.subwindow,
                      &event.xbutton.x_root,
                      &event.xbutton.y_root,
                      &event.xbutton.x,
                      &event.xbutton.y,
                      &event.xbutton.state);
    }

    // Press button
    if (XSendEvent(display, PointerWindow, True, 0xfff, &event) == 0) {
       fprintf(stderr, "Error: Failed button down event\n");
    }

    XFlush(display);

    // Hold button down for a mullisecond
    struct timespec wait = {0, 100000000};
    while (nanosleep(&wait, &wait));

    // Convert event to be a button up event
    event.type = ButtonRelease;
    event.xbutton.state = 0x100;

    // Release button
    if (XSendEvent(display, PointerWindow, True, 0xfff, &event) == 0) {
       fprintf(stderr, "Error: Failed button up event\n");
    }

    XFlush(display);

}

void usage_on_failure() {
    fprintf(stderr, "ERROR: Invalid arguments!!!\n");
    fprintf(stderr, "  Usage: fakeClick x_pos y_pos\n");
    fprintf(stderr, "         where x_pos & y_pos both >= 0\n");

    exit(EXIT_FAILURE);
}

int main(int argc, char * argv[]) {

    // Parse arguments
    if ( argc != 3 ) usage_on_failure();

    int x = atoi(argv[1]);
    int y = atoi(argv[2]);

    if ( x <= 0 || y <= 0 ) usage_on_failure();

    // Open display, exit on failure
    Display *display = XOpenDisplay(NULL);

    if (display == NULL)
    {
        fprintf(stderr, "Error in opening display!!!\n");
        exit(EXIT_FAILURE);
    }

    Window root = DefaultRootWindow(display);

    // Move mouse pointer to default position x,y 
    // on the default root window or the display.
    XWarpPointer(display, None, root, 0, 0, 0, 0, x, y);

    // Fake left mouse button single click
    mouseClick(display, Button1);  // Button1 defined in X11/X.h

    // Clean up
    XFlush(display);
    XCloseDisplay(display);

    return 0;

}
