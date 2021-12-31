# Sway notes

## Key Bindings

By default, the Super key is the main key to communicate
directly with the Sway WM.  Can be changed.

### Action Keybindings

* Open terminal:              Super+Return
* Open program:               Super+D
* Reload sway:                Super+Shift+C
* Exit sway:                  Super+Shift+E
* Quit open window:           Super+Shift+Q
* Move window to scratchpad:  Super+Shift+-
* Show scratchpad             Super+-

### Workspace Navigation Keybindings

* Go to workspace:           Super+[0..9]
* Move window to workspace:  Super+Shift+[0..9]
 
### Container Layout Keybindings

* Horizontal layout:    Super+B
* Vertical layout:      Super+V
* Stacking layout:      Super+S
* Toggle split layout:  Super+E
* Tabbed layout:        Super+W
 
### Container/Window Focus Keybindings

* Focus on parent container:   Super+A
* Swap focus tiling/floating:  Super+Space
* Toggle floating mode:        Super+Shift+Space
* Move current window focus:   Super+[Left|Right|Up|Down]
* Move focused window:         Super+Shift+[Left|Right|Up|Down]

### Container Resizing Keybindings

* Enter resize mode:  Super+R
* Resize container:   [Left|Right|Up|Down]
* Exit resize mode:   [Return|Esc]

## Factoids

* Mouse focus out of the box
  * Creating new terminal Super+Return
    * Focus jumps to new terminal window
    * Mouse stay where it was
    * Mouse focus again when mouse moved out of old window
  * I'd say sloppy mouse focus (hard to say on a tiling WM)
  * Pleasantly surprise 
    * Did not expect this behavior on keyboard oriented WM
  * Can resize windows with mouse
* Windows stack horizontally out of the box
  * Super+e will toggle between horizontal and vertical stacking
* 10 dynamic workspaces
  * Disappear if empty when you move off of them
  * Super+0 takes you to workspace 10
  * They don't renumber themselves


