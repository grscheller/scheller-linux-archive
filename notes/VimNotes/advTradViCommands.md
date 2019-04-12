# More advanced traditional vi commands
Many of the following commands existed in the original vi.
A good vi cheatsheet for traditional vi can be found here: 
[Lagmonster](http://www.lagmonster.org/docs/vi2.html).

(TL;DR) The original vi was often called "bimodal" where
_Normal Mode_ and _Command Mode_ where conflated together
and called "_Command Mode_."  _Insert Mode_ was the second
mode.  It was the Berkley Unix nvi (for new vi) which first
introduced multiple windows.

Where the behavior differs from the original Vi, I will indicate
the Vim behavior.

## _Normal Mode_ Commands:
### Misc commands
| Command    | Description                                |
|:----------:|:------------------------------------------ |
| `<ctrl-g>` | show filename and other useful status info |
| `<ctrl-l>` | redraw view                                |
| `ZZ`       | save changes and exit vim                  |
| `<ctrl-z>` | suspend vim to shell background            |

For `<ctrl-z>`, in bash usage `fg %1` will usually work to
unsuspend vim.  If you have other things suspended, hunt for it
using `$ jobs`.

### Commands to move cursor
| Command    | Description                                |
|:----------:|:------------------------------------------ |
| `+`        | move to first nonspace character next line |
| `-`        | move to first nonspace character prev line |
| `nG`       | move to nth line in file                   |
| `G`        | move to last line in file                  |
| `ngg`      | move to nth line in file                   |
| `gg`       | move to first line in file                 |
| `n\|`      | move to nth column in line                 |
| `\|`       | move to beginning of line                  |
| `0`        | move to beginning of line                  |
| `H`        | move to top of screen                      |
| `M`        | move to middle of screen                   |
| `L`        | move to bottom of screen                   |
| `nH`       | move to nth line from top of screen        |
| `nL`       | move to nth line from bottom of screen     |
| `<ctrl-u>` | move cursor/view up half a screen          |
| `<ctrl-d>` | move cursor/view down half a screen        |
| `<ctrl-b>` | move cursor/view up a full screen          |
| `<ctrl-f>` | move cursor/view down a full screen        |
| `%`        | move between matching ( ), [ ], { }, < >   |

When scrolloff is set, some of these commands get modified.

In my .vim/vimrc file, I use
```
   set scrolloff=3
```
to keep the cursor 3 lines from the edge of the screen.

### Commands to move screen view
| Command    | Description                            |
|:----------:|:-------------------------------------- |
| `<ctrl-e>` | move view down one line                |
| `<ctrl-y>` | move view up one line                  |
| `zt`       | make current line top line of view     |
| `zz`       | make current line middle line of view  |
| `zb`       | make current line bottom line of view  |
| `<nn>zt`   | make line `<nn>` top line of view      |
| `<nn>zz`   | make line `<nn>` middle line of view   |
| `<nn>zb`   | make line `<nn>` bottom line of view   |

Where applicable, you can type a number before these commands
to repeat them that many times.

### Cursor commands useful for written text
| Command | Description                                 |
|:-------:|:------------------------------------------- |
| `(`     | move cursor to beginning of sentence        |
| `)`     | move cursor to beginning of next sentence   |
| `{`     | move cursor up a paragraph                  |
| `}`     | move cursor down paragraph                  |
| `[[`    | move cursor to beginning previous section   |
| `]]`    | move cursor to beginning next section       |

What "section" means is most easily understood in the
context of file types.  For text files, different sections
can be separated by formfeeds (U+000c). `[[` and `]]`
jump you to the previous and next one respectively.

(TL;DR) For pre-ANSI K&R C files, The last two will jump
between `{` which are in the first column.  Programmers
used these to jump between C functions in source code.
For troff files various constructs were understood as
defining "sections."

### Commands to change text
| Command    | Description                                               |
|:----------:|:--------------------------------------------------------- |
| `C`        | change from cursor to end of line (enter _Insert Mode_)   |
| `R`        | from cursor, overwriting text (enter _Replace Mode_)      |
| `S`        | change entire line (enter _Insert Mode_)                  |
| `I`        | insert text at beginning of line after initial whitespace |
| `i`        | enter _Insert Mode_                                       |
| `a`        | advance cursor one char and enter _Insert Mode_           |
| `A`        | advance cursor to end of line and enter _Insert Mode_     |
| `x`        | delete char at cursor, stay in _Normal Mode_              |
| `X`        | delete char before cursor, stay in _Normal Mode_          |
| `>>`       | move entire line 1 tabstop right, stay in _Normal Mode_   |
| `<<`       | move entire line 1 tabstop left, stay in _Normal Mode_    |

## _Insert Mode_ Commands:
| Command          | Description                                       |
|:----------------:|:------------------------------------------------- |
| `<ctrl-h>`       | delete previous character                         |
| `<backspace>`    | delete previous character                         |
| `<ctrl-v> <chr>` | take <chr> literally                              |
| `<ctrl-w>`       | delete previous word                              |
| `<ctrl-c>`       | break out of _Insert Mode_, punt on any auto cmds |
| `<ctrl-x>`       | enter _Insert Mode_ completion submode            |

For more information on `<ctrl-x>` see,
```
   :help ins-completion
```
If you accidentally typed `<ctrl-x>` while in insert mode, typing any
non-control character will get you back.  If you have terminal flow
control turned on, and you hit the unfortunate key combination
`<ctrl-x> <ctrl-s>`, something EMACS users are likely to do, you will
find your vim editting session frozen.  Type `<ctrl-q>` to unlock.

### _Insert Mode_ vs _Replace Mode_:
* _Replace Mode_ is similar to _Insert Mode_ but
  characters are overwritten instead of inserted.
* You can toggle between them via the terminal
  `<insert>` key. 
* You can enter _Replace Mode_ directly from _Normal Mode_
  via the `R` command.
* Like in _Insert Mode_ you can naviagte around the text
  via the arrow keys creating multiple undo events.
* In _Replace Mode_, the `<backspace>` and `<ctrl-h>` act
  like a back arrow key but undoes (only) last set of replacements.

## _Command Mode_ Commands:
| Command        | Description                                          |
|:-------------- |:---------------------------------------------------- |
| `:r file`      | read file and insert it after current line           |
| `:nr file`     | read file and insert it after line n                 |
| `:w!`          | write file overriding normal checks                  |
| `:n,mw file`   | save lines n thru m to file                          |
| `:n,mw >>file` | append lines n thru m to existing file               |
| `:'a,'bw file` | save lines from line with mark a to line with mark b |
| `:e!`          | reedit file discarding any unsaved changes           |
| `:#`           | show current line number and print line              |
| `:.=`          | show current line number                             |
| `:=`           | show number of lines in buffer                       |
| `:n,md`        | delete lines n thru m

## Marks:
Marks allow you to set locations to either be able to jump to
or use with _Normal Mode_ editing commands.

Marks within a given buffer are denoted via leters `a-z`.  For marks between
different buffers, use letters `A-Z`.  The mark is a "zero-width" entity
between the cursor and the preceding character.

| Command   | Description                                                  |
|:---------:|:------------------------------------------------------------ |
| `ma`      | set mark `a` for the current editing buffer                  |
| `mB`      | set mark `B` for all buffers                                 |
| `` `a ``  | jump to mark `a` current buffer                              |
| `` `B ``  | jump to mark `B` current or another editing buffer           |
| `'a`      | jump to first nonspace char in line with mark `a`            |
| `` d`a `` | delete from cursor to mark `a`                               |
| `` y`a `` | yank from cursor to mark `a`                                 |
| `` y`B `` | yank from cursor to mark `B`, fails if not in current buffer |
| `d'w`     | deletes current line thru line with mark `w`                 |

Like a mark, the cursor is also a "zero-width" entity between the 
highlighted character and the preceeding character.  If the mark is
before the cursor in the file, the selection does not contain the
highlighted character.  Just like the behavior of the `yb` _Normal Mode_
command.

