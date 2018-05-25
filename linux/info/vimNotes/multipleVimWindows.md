## Using multiple windows within a terminal 
Within a single vim terminal editing session, using multiple
CLI ncurses windows can be very useful.  For example,
using the help facility `:help <command>` can be confusing
if you don't know how to navigate multiple vim windows.

### Basic concepts
* A _file buffer_ is the in memory text associated with a file
* A _window_ is a viewport on a buffer
* A _tab page_ is a collection of windows

### Creating new windows _Command Mode_

| Command | Description                             |
|:------- |:--------------------------------------- |
| `:new`  | new window with empty buffer above      |
| `:vnew` | new window with empty buffer to left    |
| `:spl`  | new window new view same buffer above   |
| `:vspl` | new window new view same buffer to left |

### Creating new windows _Normal Mode_

| Command     | Description                             |
|:-----------:|:--------------------------------------- |
| `<ctrl-w>n` | new window with empty buffer above      |
| `<ctrl-w>s` | new window new view same buffer above   |
| `<ctrl-w>v` | new window new view same buffer to left |

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

### Adjusting windows size (without mouse support):

#### Equalize window size

| Command        | Description                            |
|:--------------:|:-------------------------------------- |
| `<ctrl-w>=`    | equalize heights/widths of all windows |

#### Setting/adjusting window sizes

| Command       | Description                             |
|:-------------:|:--------------------------------------- |
| `20<ctrl-w>_` | set active window height 20 lines       |
| `72<ctrl-w>|` | set active window width 72 chars        |
| `10<ctrl-w>+` | increaces active window height 10 lines |
| `15<ctrl-w>-` | decreaces active window height 15 lines |
| `10<ctrl-w>>` | increaces active window width 10 char   |
| `15<ctrl-w><` | decreaces active window width 15 char   |

Also, note that

| Command     | Description                   |
|:-----------:|:----------------------------- |
| `<ctrl-w>_` | maximize active window height |
| `<ctrl-w>|` | maximize active window width  |

but

| Command     | Description                            |
|:-----------:|:-------------------------------------- |
| `<ctrl-w>+` | increaces active window height 1 lines |
| `<ctrl-w>-` | decreaces active window height 1 lines |
| `<ctrl-w>>` | increaces active window width 1 char   |
| `<ctrl-w><` | decreaces active window width 1 char   |

