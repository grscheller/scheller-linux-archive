## More advanced traditional vi commands
Many of the following commands existed in the original vi.
A good vi cheatsheet for traditional vi can be found here: 
[Lagmonster](http://www.lagmonster.org/docs/vi2.html).

(TL;DR) The original vi was often called "bimodal" where
_Normal Mode_ and _Command Mode_ where conflated together
and called "_Command Mode_."  _Insert Mode_ was the second
mode.  It was the Berkley Unix nvi (for new vi) which first
introduced multiple windows.

### _Normal Mode_ Commands:
#### Status commands
| Command  | Description                                |
|:----------:|:------------------------------------------ |
| `<ctrl-g>` | show filename and other useful status info |

#### Commands to move cursor
| Command    | Description                                |
|:----------:|:------------------------------------------ |
| `+`        | move to first nonspace character next line |
| `-`        | move to first nonspace character prev line |
| `1G`       | move to 1st line in file                   |
| `G`        | move to last line in file                  |
| `nG`       | move to nth line in file                   |
| `\|`       | move to beginning of line                  |
| `n\|`      | move to nth column in line                 |
| `H`        | move to top of screen                      |
| `M`        | move to middle of screen                   |
| `L`        | move to bottom of screen                   |
| `nH`       | move to nth line from top of screen        |
| `nL`       | move to nth line from bottom of screen     |
| `<ctrl-u>` | move cursor up half a screen               |
| `<ctrl-d>` | move cursor down half a screen             |
| `<ctrl-b>` | move cursor up a full screen               |
| `<ctrl-f>` | move cursor down a full screen             |
| `%`        | move between matching ( ), [ ], or { }     |

#### Commands to move screen view
Where applicable, you can type a number before these commands
to repeat them that many times.

| Command    | Description                                |
|:----------:|:------------------------------------------ |
| `<ctrl-e>` | move view down one line                    |
| `<ctrl-y>` | move view up one line                      |
| `<ctrl-u>` | move view up half a screen                 |
| `<ctrl-d>` | move view down half a screen               |
| `<ctrl-b>` | move view up a full screen                 |
| `<ctrl-f>` | move view down a full screen               |
| `<ctrl-l>` | redraw view                                |
| `z<ret>`   | make current line top line of view         |
| `nz<ret>`  | make line n top line of view               |
| `z.`       | make current line middle line of view      |
| `nz.`      | make line n middle line of view            |
| `z-`       | make current line bottom line of view      |
| `nz-`      | make line n bottom line of view            |

#### Cursor commands useful for written text
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

### _Insert Mode_ Commands:
| Command         | Description                                       |
|:---------------:|:------------------------------------------------- |
| `<ctrl-h>`      | delete previous character                         |
| `<backspace>`   | delete previous character                         |
| `<ctrl-v><chr>` | take <chr> literally                              |
| `<ctrl-w>`      | delete previous word                              |
| `<ctrl-c>`      | break out of _insert mode_, punt on any auto cmds |
| `<ctrl-x>`      | enter _Insert Mode_ completion submode            |

For more information on `<ctrl-x>` see,
```
   :help ins-completion
```
If you accidentally typed `<ctrl-x>`, typing any non-control character will
get you back.  If you have terminal flow control turned on, and you hit
the unfortunate key combination `<ctrl-x><ctrl-s>, something EMACS are
likely to do, you will find your vim editting session frozen.  Type
`<ctrl-q>` to unlock.

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
  like a back arrow key.

### _Command Mode_ Commands:
| Command        | Description                                       |
|:--------------:|:------------------------------------------------- |
| `:r file`      | read file and insert it after current line        |
| `:nr file`     | read file and insert it after line n              |
| `:w!`          | write file overriding normal checks               |
| `:n,mw file`   | save lines n thru m to file                       |`
| `:n,mw >>file` | append lines n thru m to existing file            |`
| `:e!`          | reedit file discarding any unsaved changes        |
| `:#`           | show current line number and print line           |
| `:.=`          | show current line number                          |
| `:=`           | show number of lines in buffer                    |
| `:n,md`        | delete lines n thru m

### Markers:


