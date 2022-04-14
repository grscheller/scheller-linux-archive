# Sway notes

## Key Bindings

By default, the Mod key is the Super key (Mod4).  It is
the main key to communicate directly with the Sway WM.

### Workspace Navigation Keybindings

* Go to workspace:          `Mod+[0..9]`
* Move window to workspace: `Mod+Shift+[0..9]`
* Shift focus floating/non-floating widows: `Mod+Space`
* Show/Hide/Cycle/Focus scratchpad widows:  `Mod+-`

### Action Keybindings

* Open terminal:             `Mod+Return`
* Open program:              `Mod+d`
* Reload sway:               `Mod+Shift+c`
* Exit sway:                 `Mod+Shift+e`
* Quit open window:          `Mod+Shift+q`
* Send window to scratchpad: `Mod+Shift+-`
* Toggle window floating/nonfloating: `Mod+Shift+Space`

### Container Layout Keybindings

* Horizontal layout (left to right):   `Mod+b`
* Vertical layout (top to bottom):     `Mod+v`
* Toggle horizontal/vertical layout:   `Mod+e`
* Tabbed layout (tabbed left to rt):   `Mod+w`
* Stacking layout (top bars stacked ): `Mod+s`

### Container Focus Keybindings

* Focus on parent container:  `Mod+a`
* Swap focus tiling/floating: `Mod+Space`
* Toggle floating mode:       `Mod+Shift+Space`
* Move current window focus:  `Mod+[Left|Right|Up|Down]`
* Move focused window:        `Mod+Shift+[Left|Right|Up|Down]`

### Container Resizing Keybindings

* Enter resize mode: `Mod+r`
  * Resize container: `[Left|Right|Up|Down]`
  * Resize container: `[h|j|k|l]`
  * Exit resize mode: `[Return|Esc]`

## Alacritty terminal

### Screen clearing

* Move prompt top, retain scrollback history : `Ctrl+l`
* Clear screen & scrollback history          : `/usr/bin/clear`

## Factoids

### Initial Impressions

* Mouse focus out of the box
  * Creating new terminal `Super+Enter`
    * Focus jumps to new terminal window
    * Mouse stay where it was
    * Mouse focus again when mouse moved out of old window
  * I'd say sloppy mouse focus (hard to say on a tiling WM)
    * Pleasantly surprise
    * Did not expect this behavior on keyboard oriented WM
  * Can resize windows with mouse
* Windows stack horizontally out of the box
  * `Super+e` will toggle between horizontal and vertical stacking
* 10 dynamic workspaces
  * Workspaces disappear if empty when you move off of them
  * `Super+0` takes you to workspace 10
  * Workspaces don't renumber themselves

### Observations After Several Months of Use

* Containers are hierarchical
  * `Mod+a` shifts focus to parent container
  * `Mod+[hjkl]` moves container
    * through parent container
    * into sibling containers
    * then through parent's parent when you reach end of parent
  * With multiple monitors
    * can navigate between the displayed workspaces on each via Mod+[hl]
* The scratchpad best thought of as a hidden workspace
  * containing just floating containers
  * can only display one of its container at a time
    * show/hide/cycle containers via `Mod+-` 
  * can float workspace windows without putting them in scratchpad
    * `Mod+Shift+Space` floats a workspace container
    * `Mod+Shift+-` moves workspace window into scratchpad
  * unfloating a scrachpad container
    * removes it from scratchpad
    * returns it to a workspace
