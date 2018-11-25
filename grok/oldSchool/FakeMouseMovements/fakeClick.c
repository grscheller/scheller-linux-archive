/*
 *   Fake a mouse click at position x,y
 *
 *     Usage: fakeClick 1000 100
 *
 *   Positions cursor x units down & y units to right from 
 *   the upper left hand courner of screen.  Then performs
 *   a left mouse button click.
 *
 *   Got original code from Stackoverflow:
 *     https://stackoverflow.com/questions/20595716/control-mouse-by-writing-to-dev-input-mice/20772019#20772019
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

void mouseClick(int button) {
    Display *display = XOpenDisplay(NULL);

    XEvent event;

    if(display == NULL)
    {
        fprintf(stderr, "Error in opening display!!!\n");
        exit(EXIT_FAILURE);
    }

    memset(&event, 0x00, sizeof(event));

    event.type = ButtonPress;
    event.xbutton.button = button;
    event.xbutton.same_screen = True;

    XQueryPointer(display,
                  RootWindow(display,
                  DefaultScreen(display)),
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

    if (XSendEvent(display, PointerWindow, True, 0xfff, &event) == 0) {
       fprintf(stderr, "Error\n");
    }

    XFlush(display);

    usleep(100000);

    event.type = ButtonRelease;
    event.xbutton.state = 0x100;

    if (XSendEvent(display, PointerWindow, True, 0xfff, &event) == 0) {
       fprintf(stderr, "Error\n");
    }

    XFlush(display);
    XCloseDisplay(display);
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

    Display *display = XOpenDisplay(0);
    Window root = DefaultRootWindow(display);

    // Move mouse pointer to default position x,y 
    // on the default root window or the display.
    XWarpPointer(display, None, root, 0, 0, 0, 0, x, y);

    // Fake left mouse button single click
    mouseClick(Button1);  // Where does Button1 come from?

    // Clean up
    XFlush(display);
    XCloseDisplay(display);

    return 0;

}
