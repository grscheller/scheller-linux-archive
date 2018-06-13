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

