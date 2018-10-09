## Useful and/or essential Vim factoids

### Buffers and registers in Vim
These are areas that can store text.  The three most important ones
for now are:
* default register
* named registers
* file buffers

For more in depth information see the section on types of registers in
[vimSpecificFeatures](vimSpecificFeatures.md#types-of-registers),

#### The default register
The default register is just the area which `y` and `c` commands
write to and `p` and `c` commands read from by "default."

#### Named registers
Illustrated in [Basic text editing](basicTextEditing.md#you-can-use-named-registers-to-store-text),
Named registers are areas where you can store snippets of text.
They are named `"a` thru `"z` and are essentially 26
independent "clip boards" that are shared between all the
file buffers.

#### File buffers
These are the in memory text associated with a file.  To list
them, use the `:buffers` command.  Among other things, vim
gives a unique buffer number and associates a filename (if any) 
with that buffer.

| Command       | Description                                 |
|:------------- |:-------------------------------------------------------- |
| `:n`          | edit next buffer                                         |
| `:next`       | same as above                                            |
| `:prev`       | edit previous buffer                                     |
| `:edit file`  | edit buffer associated with file in current window       |
| `:e file`     | same as above, for both, creates new buffer if necessary |
| `:buffers`    | list buffers                                             |
| `:ls`         | same as above, not the same as `!ls`                     |
| `:b2`         | edit buffer 2 in current window (not commonly used)      |
| `:new`        | open new window with a new empty buffer                  |
| `:split`      | open new window but use the same buffer                  |
| `:spl`        | same as above, basically 2 views of same buffer          |
| `:w`          | write buffer to file associated with buffer              |
| `:w file`     | write buffer to file, buffer file association unchange   |
| `:q`          | quit window, fails if last view & changes not saved      |
| `:q!`         | quit window, abandon any changes if last view            |

#### Numbered registers
My advise to beginners, just forget they exist.  Not to be confused with
the unique number vim gives file buffers.

They have names like `""`, `"0`, `"1`, `"2`, `"3`, `"4`, ... `"9`

The `""` buffer is actually the name of the default buffer.

The `"0` buffer contains the last whole line yank.

The `"1` thru `"9` buffers act like a stack containing deleted or substituted
whole line text.

### Using the mouse
When configured to use the mouse, vim will steal the mouse
events from the terminal emulator.  To enable full vim mouse
support, `:set mouse=a` and to disable the mouse and let the
terminal emulator handle all mouse events, `:set mouse=`.

Available mouse options are:

| Option | Mode                                 |
|:------:|:------------------------------------:|
| `n`    | _Normal Mode_                        |
| `v`    | _Visual Mode_                        |
| `i`    | _Insert Mode_                        |
| `c`    | _Command Mode_                       |
| `a`    | All previous modes                   |
| `h`    | All previous modes only when in help |

As a workaround, you can send mouse events directly to the
terminal emulator instead of Vim by holding down the SHIFT
key.

I find that configuring the mouse for anything but _Normal Mode_
pretty useless and counter intuitive.  In _Insert Mode_ I
don't like the middle mouse button repositioning a paste.
In _Normal Mode_ I don't like a click-drag throwing me into
character _Visual Mode_.

### Configuring wildmenu
To make tab completion in command mode more efficient, put the
following lines in your ~/.vim/vimrc or ~/.vimrc file.
```
   set wildmenu
   set wildmode=longest:full,full
```
   
### Vi and Vim differences:
* Vi only has one level of undo/redo, `<ctrl-u>` undo the
  last change and, if hit again, will redo the change.
  `<ctrl-r>` has no effect.
* On modern Linuxes, the vi "executable" is either a
  symlink to ex, traditional BSD based vi, or a symlink
  to vim.  If vim is started with the name vi, it launches
  itself in vi compatibility mode.  Vim in compatibility
  mode is neither POSIX compliant nor a Vi clone.
* In vi, you cannot navigate around file in _Insert Mode_ or _Replace Mode_
  with the arrow keys.  Hitting `<ins>` while in these modes
  does not swap you between them.
