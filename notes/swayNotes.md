# Sway notes

## Key Bindings

By default, the `Mod` key is the `Super` key (`Mod4`).
It is the canonical key to communicate directly with the Sway WM.

### Workspace Navigation Keybindings

| Keybinding         | Description                              |
|:------------------ |:---------------------------------------- |
| `Mod+[0..9]`       | Go to workspace                          |
| `Mod+Shift+[0..9]` | Move window to workspace                 |
| `Mod+Space`        | Shift focus floating/non-floating widows |
| `Mod+-`            | Show/Hide/Cycle/Focus scratchpad widows  |

### Action Keybindings

| Keybinding        | Description                        |
|:----------------- |:---------------------------------- |
| `Mod+Return`      | Open terminal                      |
| `Mod+d`           | Open program                       |
| `Mod+Shift+c`     | Reload sway                        |
| `Mod+Shift+e`     | Exit sway                          |
| `Mod+Shift+q`     | Quit open window                   |
| `Mod+Shift+-`     | Send window to scratchpad          |
| `Mod+Shift+Space` | Toggle window floating/nonfloating |

### Container Layout Keybindings

| Keybinding | Description                        |
|:---------- |:---------------------------------- |
| `Mod+b`    | Horizontal layout (left to right)  |
| `Mod+v`    | Vertical layout (top to bottom)    |
| `Mod+e`    | Toggle horizontal/vertical layout  |
| `Mod+w`    | Tabbed layout (tabbed left to rt)  |
| `Mod+s`    | Stacking layout (top bars stacked) |

### Container Focus Keybindings

| Keybinding         | Description                |
|:------------------ |:-------------------------- |
| `Mod+a`            | Focus on parent container  |
| `Mod+Space`        | Swap focus tiling/floating |
| `Mod+Shift+Space`  | Toggle floating mode       |
| `Mod+[hjkl]`       | Move current window focus  |
| `Mod+Shift+[hjkl]` | Move focused window        |

### Container Resizing Keybindings

| Keybinding | Description       |
|:---------- |:----------------- |
| `Mod+r`    | Enter resize mode |
| `[hjkl]`   | Resize container  |
| `[Return]` | Exit resize mode  |
| `[Esc]`    | Exit resize mode  |

## Alacritty terminal

### Screen clearing

| Keybinding       | Description                                |
|:---------------- |:------------------------------------------ |
| `Ctrl+l`         | Move prompt top, retain scrollback history |
| `/usr/bin/clear` | Clear screen & scrollback history          |

## Factoids

* Containers are hierarchical
  * `Mod+a` shifts focus to parent container
  * `Mod+[hjKl]` moves focus to next container in that direction
    * thru parent container
    * into sibling containers
    * then thru parent's parent when you reach end of parent
  * `Mod+Shift+[hjKl]` move container in that direction
    * thru parent container
    * into sibling containers
    * then thru parent's parent when you reach end of parent
* With multiple monitors
  * each monitor has separate workspaces
  * can navigate between current workspaces on adjacent displays
    * via `Mod+[hl]`
    * not true for adjacent workspaces on a display
* The scratchpad best thought of as a hidden workspace
  * containing just floating containers
  * can only display one of its container at a time
    * focus/show/hide/cycle containers via `Mod+-`
  * can float workspace containers without putting them in scratchpad
    * `Mod+Shift+Space` float or unfloat (toggle) a workspace container
    * `Mod+Shift+-` moves workspace window into scratchpad
  * unfloat a scrachpad container via `Mod+Shift+Space`
    * removes it from the scratchpad
    * returns it to a workspace
* Sloppy mouse focus
  * Creating new terminal `Mod+Enter`
    * Focus jumps to new terminal window
    * Mouse stay where it was
    * Mouse focus again when mouse moved out of old window
  * Can resize windows with mouse
  * Sometimes I'll go hours before realizing I forgot to turn mouse on
* 10 dynamic workspaces
  * Workspaces disappear if empty when you move off of them
  * Workspaces don't renumber themselves
  * Workspace number just a name, can be changed
    * `Mod+0` takes you to workspace 10
* Windows initially laid out horizontally (opposite Vim convention)
  * `Mod+e` toggle sibling containers between horizontal and vertical layout
  * `Mod+w` tab sibling containers in parent
  * `Mod+s` stack sibling containers in parent
  * `Mod+v` make current container vertically laid out
  * `Mod+b` make current container horizontally laid out

