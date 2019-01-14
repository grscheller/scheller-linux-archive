/*
 *   Simulate mouse movements to keep system alive
 *
 *     Usage: jiggleMouse [delay [timeout [deltaX [deltaT]]]]
 *
 */
#include <X11/Xlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void exit_usage(char *);

int
main(int argc, char *argv[])
{
    /* Parse cmdline arguments */
    int delay = 120;  // seconds between jiggles (5 minutes)
    int timeout = 0;  // no timeout
    int deltaX = 1;   // amount of XWin-coords to "jiggle the mouse"
    int deltaT = 0;  // fast as possible
    bool countdown;

    if (argc > 5) {
        exit_usage("invalid number of arguments");
    } else if (argc == 5) {
        delay = atoi(argv[1]);
        timeout = atoi(argv[2]);
        deltaX = atoi(argv[3]);
        deltaT = atoi(argv[4]);
    } else if (argc == 4) {
        delay = atoi(argv[1]);
        timeout = atoi(argv[2]);
        deltaX = atoi(argv[3]);
    } else if (argc == 3) {
        delay = atoi(argv[1]);
        timeout = atoi(argv[2]);
    } else if (argc == 2) {
        delay = atoi(argv[1]);
    }

    if (timeout == 0)
        countdown = False;
    else {
        countdown = True;
        if (deltaT > timeout)
            exit_usage("deltaT too large for timeout");
    }

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
                     0, 0, 0, 0, root_x+deltaX, root_y+deltaX);
        XFlush(display);
        if (deltaT > 0)
            sleep(deltaT);
        XWarpPointer(display, None, *(root_windows+ii),
                     0, 0, 0, 0, root_x, root_y);
        XFlush(display);

        /* Sleep and setup for next jiggle */
        sleep(delay);
        if (countdown)
            timeout = timeout - delay + deltaT;
    }

    free(root_windows);
    XCloseDisplay(display);

    exit(EXIT_SUCCESS);
}

void
exit_usage(char *msg)
{
    fprintf(stderr, "Error: %s\n", msg);
    fprintf(stderr, "Usage: jiggleMouse [delay [timeout [deltaX [deltaT]]]\n\n");
    fprintf(stderr, "  delay  = time in seconds between jiggles\n");
    fprintf(stderr, "  timeout = end program after timeout seconds\n");
    fprintf(stderr, "  deltaX = distance to jiggle\n");
    fprintf(stderr, "  deltaT = time it takes to jiggle\n\n");
    fprintf(stderr, "where timeout > deltaT when timeout > 0,\n");
    fprintf(stderr, "      timeout = 0 means no timeout\n");

  exit(EXIT_FAILURE);
}
