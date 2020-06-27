## Purpose: Gnome 3 configuration on Arch Linux

### GNOME Version:
```
   $ gnome-shell --version
   GNOME Shell 3.36.3
```
### gnome-tweak (Tweaks)
* General -> Animations (on)
* General -> Over-Amplification (on)
* Appearance -> Themes -> Applications -> Adwaita-dark
* Appearance -> Background: to select background image
* Appearance -> Lock Screen: to select background image
* Extensions -> Clipboard indicator (on)
* Extensions -> Clipboard indicator -> Settings
* Fonts: Made adjustments
* Keyboard & Mouse -> Mouse -> Pointer Location (on)
* Keyboard & Mouse -> Mouse -> Middle Click Paste (on)
* Keyboard & Mouse -> Touchpad -> Disable While Typing (on)
* Keyboard & Mouse -> Touchpad -> Mouse Click Emulation -> Area
* Startup Applications -> Added "ignore-lid-switch-tweak"
* Top Bar -> Activies Overview Hot Corner (off)
* Top Bar -> Battery Percentage (on)
* Top Bar -> Clock -> Weekday (on)
* Top Bar -> Clock -> Date (on)
* Top Bar -> Calendar -> Week Numbers (off)
* Window Titlebars -> Titlebar Actions -> Double-Click -> Toggle Maximize
* Window Titlebars -> Titlebar Actions -> Middle-Click -> Lower
* Window Titlebars -> Titlebar Actions -> Secondary-Click -> Menu
* Window Titlebars -> Titlebar Buttons -> Maximize (off)
* Window Titlebars -> Titlebar Buttons -> Minimize (on)
* Window Titlebars -> Titlebar Buttons -> Placement -> Right
* Windows -> Attach Modal Dialogs (on)
* Windows -> Edge Tiling (on)
* Windows -> Window Focus -> Focus on Hover (on)
* Windows -> Raise Windows When Focused (off)
* Workspaces -> Dynamic Workspaces
* Workspaces -> Display Handling -> Workspaces span displays

### gnome-control-center (Settings)
* Wifi & Network(Ethernet) configurable
* Bluetooth configurable
* Set background and lock screen images
* Notifications configuration
* Search configuration
* Online Account configuration
* Sound configuration
* Power settings
* Devices
* Details

### Configurable via on command line too
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
### Layout
* Activities Button: LHS Top Toolbar
  - switches between normal and overview mode
  - click, mouse to upper LH corner, super-key up event
* Dash: Vertical toolbar on LHS screen (overview mode)
  - Docky like
* Menu Dropdown: right of Activities Button
  - for active window
  - Mac like
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
* super+tab: switch between windows on current desktop
* super+L: lock screen
* super key-up-event: switches between the desktop and overview mode
* super+up-arrow: maximize window with focus 
* super+down-arrow: regular size window
* super+left-arrow: tile window with focus LHS workspace
* super+right-arrow: tile window with focus RHS workspace
* super+H: hide (minimize) window
* super+click: raise window
* click titlebar: raise window
* middle-click titlebar: drop window below all other windows
* double-click titlebar: toggle maximize window
* ctrl+alt+up-arrow: move up one workspace
* ctrl+alt+down-arrow: move up one workspace

### Paradigms
* No desktop icons
* Desktop acts as a window manager
* The Dash acts like a program manager
  * make sure "launch new instance" turned on
  * right-click to select amoung running instances

### Gnome-Terminal
* Create shortcut
  * Settings => Keyboard => Custom Shortcuts(at bottom) => +
    * Name: Terminal
    * Command: gnome-terminal
    * Shortcut: ctrl+alt+T
* Change Terminal Colors
  * Click profile (Geoffrey)
    * Colers Tab
    * Uncheck "Use colors from system theme"
    * Palette: Linux console
    * Default color
        * Text: White (lower right most)
        * Background: Black (upper left most)
      * Check Bold color: Yellow (bottom 4th from left)
      * Uncheck Cursor color
      * Check Highlight color
        * Text: Black
        * Background: Gray (upper right most)
    * Check Show bold text in bright colors
