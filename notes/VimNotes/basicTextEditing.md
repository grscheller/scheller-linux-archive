# Basic text editing by example
This should be enough to enable you to be productive with vim.
For many years, this is basically all I knew.  Vim, like EMACS,
is a very power editor.  These examples barely scratch the surface
of what it can do.

(TL;DR) Vim is actually a Turing complete language.  It can be turned
into a complete IDE with full Unix Shell support.

I think with a few weeks of practice, the material covered here
can be internalized and eventually become part of your "muscle memory."

## Vim has 4 main modes:
* _Normal Mode_
* _Command Mode_
* _Insert Mode_
* _Visual Mode_ (not in original vi)

### Cursor movement in _Normal Mode_:

| Command        | Description                                      |
|:--------------:|:------------------------------------------------ |
| `h,i,j,k`      | move cursor one character (also arrow keys)      |
| `w, W`         | move forward to beginning next word              |
| `b, B`         | move back to beginning word                      |
| `e, E`         | move forward to end of word                      |
| `$`            | move to end of line                              |
| `^`            | move to first non-whitespace character on line   |
| `0`            | move to beginning of line                        |
| `G`            | move to last line in file                        |
| `gg`           | move to first line in file                       |
| `f<char>`      | move forward to next <char> on current line      |
| `F<char>`      | move backward to next <char> on current line     |
| `t<char>`      | move forward before next <char> on current line  |
| `T<char>`      | move backward after next <char> on current line  |
| `;`            | move forward to target of last f command         |
| `,`            | move backward to target of last f command        |
| `3fw     `     | move forward to 3rd w on current line            |
| `/RegExp<ret>` | forward search for regular expression pattern    |
| `?RegExp<ret>` | backward search for regular expression pattern   |
| `/<ret>`       | search forward for last pattern                  |
| `?<ret>`       | search backward for last pattern                 |
| `n`            | search forward or backward for last pattern      |
| `N`            | search for last pattern in reverse sense to `n`  |

### Interacting with "the buffer" in _Normal Mode_:
Buffer is older vi jargon for what is now called the
default register in vim.

| Command       | Description                                      |
|:-------------:|:------------------------------------------------ |
| `yy`          | yank line to buffer (copy)                       |
| `dd`          | delete line and put in buffer (cut)              |
| `5dd  `       | delete 5 lines and put in buffer                 |
| `x`           | delete character under cursor to buffer          |
| `~`           | change case of current char and advance one char | 
| `r<char>`     | change current char to <char>                    |
| `p`           | paste buffer contents "after"                    |
| `P`           | paste buffer contents "before"                   |

What "before" or "after" mean depends on what is
in the buffer.  `y` and `d` can be used with all
the _normal mode_ cursor positioning commands.

| Command | Description                                                |
|:-------:|:---------------------------------------------------------- |
| `d$`    | delete to end of line and put in buffer                    |
| `d0`    | delete everything before cursor on line and put in buffer  |
| `3yw`   | yank three words to buffer, starting at cursor             |
| `y^`    | yank everything before cursor to first non-whitespace char |
| `d2fz`  | delete from cursor to 2nd z on current line                |
| `2db`   | delete 2 previous words starting from cursor               |
| `2y3w`  | ends up yanking 6 words                                    |
| `5x`    | delete next 5 characters on current line                   |
| `5X`    | delete previous 5 characters on current line               |

### You can use named "buffers" to store text
In vim these now are refered to as named registers.

| Command | Description                                                |
|:-------:|:---------------------------------------------------------- |
| `"adw`  | delete word and put in buffer` "a`                        |
| `"B2yy` | yank 2 lines and append to buffer `"b`                     |
| `"sd$`  | delete to end of line and put in buffer `"s`               |
| `"sp`   | paste contents of buffer `"s` after cursor                 |
| `"aP`   | paste contents of buffer `"a` before cursor                |

One use case for named registers is copying multiple items
from multiple files and pasting them into other files.

### Commands to insert or manipulate text:
These commands take vim from _Normal Mode_ to _Insert Mode_.
To return to _Normal Mode_, type either `<esc>` or `<ctrl-[>` .

| Command | Description                                                |
|:-------:|:---------------------------------------------------------- |
| `i`     | insert text before cursor                                  |
| `a`     | insert text after cursor                                   |
| `I`     | insert text at beginning of line after initial white space |
| `0i`    | insert text beginning of line                              |
| `A`     | insert text at end of line                                 |
| `o`     | open new line after current line to insert text            |
| `O`     | open new line before current line to insert text           |
| `S`     | delete line contents and enter Normal Mode                 |
| `3cw`   | change next three words                                    |
| `c3w`   | change next three words                                    |
| `5cc`   | change next 5 lines                                        |
| `2cb    | change previous 3 words                                    |
| `c$     | change to end of line                                      |
| `c^`    | change text before cursor, excluding initial white space   |
| `c0`    | change text before cursor to beginning of line             |
| `s`     | delete current character and enter Normal Mode             |
| `"a3S`  | delete 3 lines into `"a` and enter Normal Mode on new line |


While in _Insert Mode_, the file can be navigated via with the arrow keys.
Text can also be deleted with the backspace key.  In _Normal Mode_, the
backspace key is just another navigation key.

### _Command Mode_ (line editor) commands:
(TL;DR) Vim is an open source version of the Unix editor vi,
which is a CLI visual version of the Berkeley Unix
line editor ex, which itself is a re-implementation of
the AT&T Unix line editor ed.
On really old terminals, essentially line printers with
keyboards, the descendants of teletypes, you edited files
one line at a time.

_Command Mode_ commands developed from the original
line editing commands.

Use the `:` command to enter _Command Mode_.  The
cursor jumps down to the bottom of the terminal window
and prompts you with `: `.

| Command             | Description                                                   |
|:------------------- |:------------------------------------------------------------- |
| `:w`                | write to disk file you are editing                            |
| `:w file`           | write to file, unlike MS Word, you still edit original file   |
| `:q`                | quit editing, vim will warn you if you have unsaved changes   |
| `:wq`               | write to disk, then quit                                      |
| `:q!`               | quit without saving unsaved changes                           |
| `:n`                | edit next buffer (typically next file given on command line   |
| `:prev`             | edit previous buffer                                          |
| `:wn`               | write to disk and move on to next file to edit                |
| `:42`               | move cursor to beginning of line 42                           |
| `:#`                | give line number of current line cursor is on                 |
| `:s/foo/bar/`       | substitute first instance of foo with bar on current line     |
| `:s/foo/bar/g`      | substitute all instances of foo with bar on current line      |
| `:17,42s/foo/bar/g` | substitute all foo with bar, lines 17 to 42                   |

While in _Command Mode_, up & down arrow keys cycle through previous
_Command Mode_ commands.  The left & right arrow keys help you re-edit the
line.  Press `<esc>` to return to _Normal Mode_ without issuing a command.

### Repeating commands in _Normal Mode_:

| Command | Description                                |
|:-------:|:------------------------------------------ |
| `.`     | repeat the last command which changed text |

This repeats the last _Normal Mode_ command used which changed text.  It
does not repeat _Command Mode_ commands.

This is frequently used in conjunction with the `n` _Normal Mode_ command.
For example, `n.n.nn.n` keeps moving to the beginning of the next match for
the last search pattern and you can either decide to repeat, or not, the
change at each location.  

### Introduction to _Visual Mode_:
This mode allows you to select region of text by visually highlighting
regions that can then be modified.

To enter _Visual Mode_ from _Normal Mode_

| Command     | Description                |
|:-----------:|:-------------------------- |
| `v`         | for character based        |
| `V`         | for line based             |
| `<ctrl-v>`  | for block visual mode      |
| `gv`        | to reselect last selection |
 
Highlight text with _Normal Mode_ cursor navigation commands
like `h, j, k, l, w, e, W, B, f` or the arrow keys.
Once selected, you can issue either _Normal Mode_ or 
_Command Mode_ commands.

_Normal Mode_ commands such as `d, y, c, I, A, >>, <<`
act on the highlighted region.  The behavior of some
commands, like indenting commands `>>` or `<<`, vary
depending on which _Visual Mode_ (character, line or block)
you are in.

_Command Mode_ commands act on the lines in their entirity
that contain the selected region.

To punt out of _Visual Mode_ without doing anything,
hit the `<esc>` key.

If you have enabled mouse support, mouse actions can cause you
to enter _Visual Mode_.  That is one reason I enable mouse
support for _Normal Mode_ only.

## Undo/redo commands:

| Command    | Description        |
|:----------:|:------------------ |
| `u`        | undo previous edit |
| `<ctrl-r>` | redo edit undone   |

These can be used to linearly undo and redo edits,
like the arrow buttons in a web browser.
Navigating with the arrow keys while in _Insert Mode_
will result in multiple entries in the undo/redo buffers.

## Some Vim command line option examples
```
   $ vim file1 file2 file3   # Open/create 3 files for editting
   $ vim +[n] file           # Open file for editing on line n (default last line of file)
   $ vim +/pattern file      # Open file for editing at first reg-exp pattern match
   $ vim -R file             # Open file read only, can still write via :w!
   $ vim -g file             # Run as gvim GUI
   $ vim -r                  # List swap files, then exit
   $ vim -r file             # Recover crashed vim session, uses swap file
   $ vim -h                  # List help message for command-line options and exit
```
## Dealing with whitespace characters:

| Command       | Description                          |
|:------------- |:------------------------------------ |
| `:set list`   | Indicate line endings & tabs         |
| `:set nolist` | Display line endings & tabs normally |

Helps when getting rid of tabs and trailing whitespace.

## Spell checking:

| Command        | Description             |
|:-------------- |:----------------------- |
| `:set spell`   | Turn spell checking on  |
| `:set nospell` | Turn spell checking off |


## Detailed help
To get started, from within vim, type
* `:help`
* `:help help`
* `:help tutorial`

Vim built in help is very powerful, but not too beginner friendly.  To get the most out of it,

* familiarize yourself with how to use [multiple vim windows](multipleVimWindows.md)
* configure the [mouse](vimFactoids.md#using-the-mouse)
* setting up the [wildmenu](vimFactoids.md#configuring-wildmenu)
* double clicking is needed to follow vim "hyperlinks"
* Use `<ctrl-o>` to jump back to previous location
* Use `<ctrl-i>` or `<tab>` to jump forward again
