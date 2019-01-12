/*
 *   Simulate mouse movements to keep system alive
 *
 *     Usage:  jiggleMouse [delay [timeout [jiggle]]]
 *
 */
#include <X11/Xlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int
main(int argc, char * argv[])
{
    /* Parse cmdline arguments */
    int jiggle = 1;   // amount of X-coords to "jiggle the mouse"
    int delay = 300;  // seconds between jiggles (5 minutes)
    int timeout = 0;  // no timeout
    bool countdown;

    if (argc > 4) {
        fprintf(stderr, "Error: invalid number of arguments\n");
        fprintf(stderr, "  Usage: jiggleMouse [delay [timeout [jiggle]]]\n");
        fprintf(stderr, "    delay  = time in seconds between jiggles\n");
        fprintf(stderr, "    jiggle = amount to jiggle\n");
        fprintf(stderr, "    timeout = end after timeout seconds\n");
        exit(EXIT_FAILURE);
    } else if (argc == 4) {
        delay = atoi(argv[1]);
        timeout = atoi(argv[2]);
        jiggle = atoi(argv[3]);
    } else if (argc == 3) {
        delay = atoi(argv[1]);
        timeout = atoi(argv[2]);
    } else if (argc == 2) {
        delay = atoi(argv[1]);
    }

    if (timeout == 0)
        countdown = False;
    else
        countdown = True;

    /* Get display or fail */
    Display *display = XOpenDisplay(NULL);
    if (display == NULL) {
        fprintf(stderr, "Error opening display!!!\n");
        exit(EXIT_FAILURE);
    }

    /* Get number of screens */
    int number_of_screens = ScreenCount(display);

    /* Allocate space for screen info and return parameters */
    Window *root_windows;
    root_windows = (Window *)malloc(sizeof(Window) * number_of_screens);
    int ii; for (ii = 0; ii < number_of_screens; ii++) {
        root_windows[ii] = RootWindow(display, ii);
    }

    Window root_return, child_return;
    int root_x, root_y, win_x, win_y;
    unsigned int mask_return;

    while(timeout >= 0) {
        /* Find current mouse position and error out if unsucessful */
        bool mouse_found;
        for (ii = 0; ii < number_of_screens; ii++) {
            mouse_found = XQueryPointer(display,
                          root_windows[ii],
                          &root_return,
                          &child_return,
                          &root_x,
                          &root_y,
                          &win_x,
                          &win_y,
                          &mask_return);
            if (mouse_found)
               break;
        }
        if (! mouse_found) {
            fprintf(stderr, "No mouse found.\n");
            exit(EXIT_FAILURE);
        }
        
        /* Jiggle the mouse */
        XWarpPointer(display, None, *(root_windows+ii),
                     0, 0, 0, 0, root_x+jiggle, root_y+jiggle);
        XFlush(display);
        XWarpPointer(display, None, *(root_windows+ii),
                     0, 0, 0, 0, root_x, root_y);
        XFlush(display);

        /* Sleep and setup for next jiggle */
        sleep(delay);
        if (countdown)
            timeout -= delay;
    }

    free(root_windows);
    XCloseDisplay(display);

    exit(EXIT_SUCCESS);
}
