# Advanced vim specific features
Vim commands not in your grandfather's Vi.

## Jump Lists:
Associated with each vim window (not buffer!) is a list of
past locations "jumped" to.  Jumps are remembered in a jump
list.  Just navigating via the `hjkl` keys will not create
jump points.  Nor will editing text.

| Command          | Description                          |
|:---------------- |:------------------------------------ |
| `:jumps`         | List jump points for active window   |
| `:help jumplist` | For more detailed info on jump lists |

The `:jumps` command will list a table consisting like this:

| jump | line | col | file/text                 |
|:----:| ----:| ---:|:------------------------- |
|  3   | 23   | 5   | previousFile.txt          |
|  2   | 2    | 11  | previousFile.txt          |
|  1   | 10   | 5   | some text in current file |
|  0   | 12   | 2   | other text current file   |
|  1   | 5    | 10  | text on line 5            |
|  2   | 3    | 5   | moreRecentFile.txt        |

Your current location in the jump list is allways 0.

| Command      | Description                                 |
|:------------:|:------------------------------------------- |
| `<ctrl-o>`   | go back to previous location in jump list   |
| `<ctrl-i>`   | go forward to next location in jump list    |
| `3 <ctrl-o>` | go back 3 jumps in jump list                |
| `2 <ctrl-i>` | go forward 2 jumps in jump list             |

## Types of registers:
### Default register
The default register has a name `""`

### Numbered registers
These contain only multiline (one or more whole lines) data.

| Register       | Purpose                                        |
|:--------------:|:---------------------------------------------- |
| `"0`           | contains contents of most recent yank command  |
| `"1`           | contains most recent delete/substitution       |
| `"2` thru `"9` | contents shift downward when `"1` is updated   |

These can be written to in Command Mode via `:let @5 = "foobar"`

### Small delete register

| Register       | Purpose                                      |
|:--------------:|:-------------------------------------------- |
| `"-`           | contains deletes/changes less than one line  |

### Named registers

| Register       | Purpose                                               |
|:--------------:|:----------------------------------------------------- |
| `"a` thru `"z` | storage registers across all file buffers             |
| `"A` thru `"Z` | same registers, used to appending instead of replace  |

### Read only registers

| Register  | Purpose                                      |
|:---------:|:-------------------------------------------- |
| `".`      | contains last inserted text                  |
| `"%`      | contains the name of the current file        |
| `":`      | contains most recent _Command Mode_ command  |

Use `:@:` to repeat last _Command Mode_ command.

### Alternate file register 
The alternate file register `"#` is an assignable name, useful when jumping
between 2 buffers via <ctrl-^>.

### Expression register
The expression register `"=` is used for expressions in commands
which use registers.

### Selection and drop registers (Interacts with Desktop GUI)

| Register  | Purpose                                  |
|:---------:|:---------------------------------------- |
| `"*`      | copy/paste from/to the X11 clipboard     |
| `"+`      | copy/paste from/to clipboard             |
| `"~`      | paste from last drag-and-drop operation  |

### Black hole register
The black hole register `"_` allows deleting text without affecting
other registers.  Reading from it returns nothing.

### Last search pattern register
The last search pattern register `"/` is readable from _Normal Mode_.
You can assign values to it in _Command Mode_ via
```
   :let @/ = "Some String"
```
