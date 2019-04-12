# Using multiple windows within a terminal 
Within a single vim terminal editing session, using multiple
CLI ncurses windows can be very useful.  For example,
using the help facility `:help <command>` can be confusing
if you don't know how to navigate multiple vim windows.

## Basic concepts
* A _file buffer_ is the in memory text associated with a file
* A _window_ is a viewport on a buffer
* A _tab page_ is a collection of windows
* A _terminal window_ is a window that displays a Shell

## Creating new windows _Command Mode_

| Command | Description                             |
|:------- |:--------------------------------------- |
| `:new`  | new window with empty buffer above      |
| `:vnew` | new window with empty buffer to left    |
| `:spl`  | new window new view same buffer above   |
| `:vspl` | new window new view same buffer to left |

## Creating new windows _Normal Mode_

| Command      | Description                             |
|:------------:|:--------------------------------------- |
| `<ctrl-w> n` | new window with empty buffer above      |
| `<ctrl-w> s` | new window new view same buffer above   |
| `<ctrl-w> v` | new window new view same buffer to left |
| `<ctrl-w> T` | break current window out in new tab     |

The directional sense of these commands can be adjusted via

| Command           | Description                            |
|:----------------- |:-------------------------------------- |
| `:set splitbelow` | open new windows below, not above      |
| `:set splitright` | open new windows to right, not to left |

I configure these in my `~/.vim/vimrc
```
   set splitbelow
   set splitright
```

## Adjusting windows size (without mouse support):

### Equalize window size

| Command         | Description                            |
|:---------------:|:-------------------------------------- |
| `<ctrl-w> =`    | equalize heights/widths of all windows |

### Setting/adjusting window sizes

| Command           | Description                             |
|:-----------------:|:--------------------------------------- |
| `20 <ctrl-w> _ `  | set active window height 20 lines       |
| `72 <ctrl-w> \|`  | set active window width 72 chars        |
| `10 <ctrl-w> +`   | increaces active window height 10 lines |
| `15 <ctrl-w> -`   | decreaces active window height 15 lines |
| `10 <ctrl-w> >`   | increaces active window width 10 char   |
| `15 <ctrl-w> <`   | decreaces active window width 15 char   |

Also, note that

| Command        | Description                   |
|:--------------:|:----------------------------- |
| `<ctrl-w> _ `  | maximize active window height |
| `<ctrl-w> \|`  | maximize active window width  |

but

| Command      | Description                            |
|:------------:|:-------------------------------------- |
| `<ctrl-w> +` | increaces active window height 1 lines |
| `<ctrl-w> -` | decreaces active window height 1 lines |
| `<ctrl-w> >` | increaces active window width 1 char   |
| `<ctrl-w> <` | decreaces active window width 1 char   |

## Tab pages:
Allows one to group related windows together.  A Tab in vim
is a collection of one or more windows.  With mouse support,
can switch between windows via clicking the "tab".

| Command           | Description                    |
|:----------------- |:------------------------------ |
| `:tabn`           | move to next tab               |
| `:tabp`           | move to previous tab           |
| `:tabnew`         | open new tab with empty buffer |
| `:tabedit <file>` | open new tab to edit <file>    |
| `gt`              | move to next tab               |
| `gT`              | move to previous tab           |

## Starting Vim with multiple windows/tabs:
```
   $ vim -p[n]    # Open n tab pages (default: one for each file)
   $ vim -o[n]    # Open n windows (default: one for each file)
   $ vim -O[n]    # Like -o but split vertically
```
## Terminal Windows
Traditionally in vi one could interact with the Unix shell via

| Command   | Description                              |
|:--------- |:---------------------------------------- |
| `:!<cmd>` | display output of shell command <cmd>    |
| `:sh`     | replace editing session with a new shell |

Vim allows you to open a shell in a separate Vim Window

| Command      | Description                                 |
|:------------ |:------------------------------------------- |
| `:term`      | open a shell in a new horizontal Vim window |
| `:vert term` | open a shell in a new vertical Vim window   |

