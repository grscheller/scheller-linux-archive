## Advanced vim specific features
Vim commands not in your grandfather's Vi.

### Jump Lists:
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

| Command     | Description                                 |
|:-----------:|:------------------------------------------- |
| `<ctrl-o>`  | go back to previous location in jump list   |
| `<ctrl-i>`  | go forward to next location in jump list    |
| `3<ctrl-o>` | go back 3 jumps in jump list                |
| `2<ctrl-i>` | go forward 2 jumps in jump list             |

### Types of registers
1. Default register has a name `""`
2. Numbered registers

   | Register       | Purpose                                        |
   |:--------------:|:---------------------------------------------- |
  `| `"0`           | contains contents of most recent yank command  |
  `| `"1`           | contains most recent multi-line delete/change  |
  `| `"2` thru `"9` | contents shift downward when `"1` changes      |
`
   These can be written to in Command Mode via `:let @5 = "foobar"`
3. Small delete register `"-` contains deletes/changes less than one line
4. Named registers `"a` thru `"z`. To append instead, use `"A` thru `"Z`
5. Read only registers

   | Register  | Purpose                                      |
   |:---------:|:-------------------------------------------- |
   | `".`      | contains last inserted text                  |
   | `"%`      | contains the name of the current file        |
   | `":`      | contains most recent _Command Mode_ command  |

   Use `:@:` to repeat last _Command Mode_ command.

6. Alternate file register `"#` assignable name, useful when jumping
   between 2 buffers via <ctrl-^> 

7. Expression register `"=` for expressions in commands which use registers

8. Selection and drop registers (Interacts with Desktop GUI)

   | Register  | Purpose                                  |
   |:---------:|:---------------------------------------- |
   | `"\*`     | copy/paste from/to the X11 clipboard     |
   | `"+`      | copy/paste from/to clipboard             |
   | `"~`      | paste from last drag-and-drop operation  |

9. Black hole register `"_`
   Allows deleting text without affecting other registers.  Reading
   from it returns nothing.

10. Last search pattern register `"/`
    Can only read from it in Normal Mode.
    Assign value while in Command Mode via `:let @/ = "Some String"`
