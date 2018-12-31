# Purpose: Gnome 3 configuration on CentOS 7

### GNOME Version:
```
   $ gnome-shell --version
   GNOME SHELL 3.30.0
```

### Configure via gsettings:
All the below can be done with gnome-tweak.  Why they don't just provide
this tweak tool with the base DE is puzzling.

* Enable sloopy focus
```
   $ gsettings set org.gnome.desktop.wm.preferences focus-mode sloppy
   $ gsettings set org.gnome.desktop.wm.preferences raise-on-click false
   $ gsettings set org.gnome.desktop.wm.preferences action-middle-click-titlebar lower
```
* Set background and screenlock images
```
   $ gsettings set org.gnome.desktop.background picture-uri file:///home/geoff/Pictures/Wallpapers/RachelHawkFalls.jpg
   $ gsettings set org.gnome.desktop.screensaver picture-uri file:///home/geoff/Pictures/Wallpapers/ErWangDongCave.jpg
```

### gnome-tweak
* Changed default theme from Adwaita to Adwaita-dark
* Turned on removable drive menu (to left of status menu)
* Tured on "Launch Mew Instance" extension
* Turned on "Clipboard Indicator" extension (after installing from AUR)

### Layout
* Activities Button: LHS Top Toolbar
  - switches between normal and overview mode
  - click, mouse to upper LH corner, super-key up event
* Dash: Vertical toolbar on LHS screen (overview mode)
  - Docky-like
* Menu Dropdown: right of Activities Button
  - for active window
  - Mac-like
* Notification Area: Center Top Toolbar
  - notifications balloons appear under it
  - day of week and time displayed
  - click to show notifications and calander
* Status Menu: RHS Top Toolbar
  - speaker volume
  - brightness control
  - wired and wifi controls
  - bluetooth control
  - log out and account settings
  - system settings
  - power off
* Work Space Switcher: vertical toolbar on RHS screen (overview mode)
  - switch workspace
  - move windows between workspaces
  - creates/deletes workspaces as needed

### Keyboard & mouse shortcuts:
* super-tab: switch between windows on current desktop
* super-L: lock screen
* super key-up-event: switches between the desktop and overview mode
* super-up-arrow: maximize window with focus 
* super-down-arrow: regular size window
* super-left-arrow: tile window with focus LHS workspace
* super-right-arrow: tile window with focus RHS workspace
* super-H: hide (minimize) window
* super-click: raise window
* click-titlebar: raise window
* middle-click-titlebar: drop window below all other windows
* double-click-titlebar: toggle maximize window

### Paradigms
No desktop icons.  Desktop acts as a window manager.  The Dash
acts like a program manager.  With the "Launch Mew Instance"
extension turned on, clicking Dash icons will launch new
instances instead of the horrible Win10 conflated behavior
of giving focus to some random already running instance.  By
right clicking, you can select amoung the running instances,
regardless of the workspace. 
