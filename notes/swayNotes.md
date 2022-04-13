# Sway notes

## Initial Observatons

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
  * Disappear if empty when you move off of them
  * Super+0 takes you to workspace 10
  * They don't renumber themselves

## Key Bindings

By default, the Mod key is the Super key (Mod4).  It is
the main key to communicate directly with the Sway WM.

Note: Capital letters represent the key, not the character.

### Action Keybindings

* Open terminal:               `Mod+Return`
* Open program:                `Mod+d`
* Reload sway:                 `Mod+Shift+c`
* Exit sway:                   `Mod+Shift+e`
* Quit open window:            `Mod+Shift+q`
* Make window a scratchpad:    `Mod+Shift+-`
* Show/Hide/Cycle scratchpads: `Mod+-`
* Focus current scratchpad:    `Mod+-`

### Workspace Navigation Keybindings

* Go to workspace:          `Mod+[0..9]`
* Move window to workspace: `Mod+Shift+[0..9]`

### Container Layout Keybindings

* Horizontal layout (left to right): `Mod+b`
* Vertical layout (top to bottom):   `Mod+v`
* Tabbed layout (tabbed left to rt): `Mod+w`
* Stacking layout (tacked top bars): `Mod+s`
* Toggle horizontal/vertical layout: `Mod+e`
 
### Container/Window Focus Keybindings

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
