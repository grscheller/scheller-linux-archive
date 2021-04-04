# Gnome 3 configuration on Arch Linux

## GNOME Version

```
   $ gnome-shell --version
   GNOME Shell 3.38.4
```

## Settings (gnome-control-center)

* Wifi & Network(Ethernet) configurations
* Bluetooth configurations
* Background configuration
* Notifications configuration
* Search configuration
* Applications
  * Can't change file associations???
  * Manage with "Software" - Flatpaks and Snaps
  * Does not handle Pacman packages
* Privacy
  * Location Services
  * Camera & Microphone
  * Thunderbolt (/sys/bus/thunderbolt not present on my laptop)
  * File History & Trash
  * Screen Lock
* Online Account configuration
* Sharing (grayed out)
* Sound configuration
* Power settings
* Displays
* Mouse & Touchpad
* Keyboard Shortcuts
  * Added `<ctrl>+<alt>+T` for gnome-terminal
  * Added `<super>+W` raise window if covered, otherwise lower it
* Printers
* Removable Media
* Color management settings for monitors and Printers
* Region & Language
* Accessibility (pretty useless - better handled via themes & plug-ins)
* Users
* Default Applications
* Date & Time (use timedatectl from cmdline instead)
* About

## Tweaks (gnome-tweak)

* General -> Animations (on)
* General -> Over-Amplification (on)
* Appearance -> Themes -> Applications -> Adwaita-dark
* Appearance -> Themes -> Cursor -> Adwaita
* Appearance -> Themes -> Icons -> Adwaita
* Appearance -> Background: select background image
* Appearance -> Lock Screen: to select background image
* Extensions -> Gpaste (on) note: must be installed (see below)
* Extensions -> Gpaste -> Settings -> General behaviour
  * Track clipboard Changes (on)
  * Close UI on select (on)
  * Enable the gnome-shell extention (on)
  * Save history
* Extensions -> Lauch new instance (on) note: makes Dash much more useful
* Extensions -> Native Window Placement (on)
* Extensions -> Places status indicator (on)
* Fonts
  * Interface & Document Text: Cantarell Regular 11
  * Monospace Text: Source Code Pro Regular 10
  * Legacy Window Titles: Cantarell Bold 11
  * Hinting: Slight
  * Antialiasing: Subpixel (for LED screens)
  * Scalting Factor: 1.00
* Keyboard & Mouse -> Keyboard -> Show Extended Input Sources (on)
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
* Window Titlebars -> Titlebar Buttons -> Maximize (on)
* Window Titlebars -> Titlebar Buttons -> Minimize (on)
* Window Titlebars -> Titlebar Buttons -> Placement -> Right
* Windows -> Attach Modal Dialogs (on)
* Windows -> Edge Tiling (on)
* Windows -> Window Action Key -> Super
* Windows -> Window Focus -> Focus on Hover (on)
* Windows -> Raise Windows When Focused (off)
* Workspaces -> Dynamic Workspaces
* Workspaces -> Display Handling -> Workspaces span displays

## Configurables not available in Gnome Settings or Gnome Tweaks

* Pass clicks to windows without raising them
  * To raise, super+click, or click on title bar

```
   $ gsettings set org.gnome.desktop.wm.preferences raise-on-click false
```

## Layout

* Activities Button: LHS Top Toolbar
  * switches between normal and overview mode
  * click, mouse to upper LH corner, super-key up event
* Dash: Vertical toolbar on LHS screen (overview mode)
  * Docky like
* Menu Dropdown: left side title bar
  * for active window
  * Mac like
* Notification Area: Center Top Toolbar
  * notifications balloons appear under it
  * day of week and time displayed
  * click to show notifications and calander
* Status Menu: RHS Top Toolbar
  * speaker volume
  * brightness control
  * wired and wifi controls
  * bluetooth control
  * log out and account settings
  * system settings
  * power off
* Work Space Switcher: vertical toolbar on RHS screen (overview mode)
  * switch workspace
  * move windows between workspaces
  * creates/deletes workspaces as needed

## Keyboard & mouse shortcuts (as I've configured)

* super key-up-event: switches between the desktop and overview mode
* super+tab: switch between windows on current desktop
* super+L: lock screen
* super+up-arrow: maximize window with focus
* super+down-arrow: regular size window
* super+left-arrow: tile window with focus LHS workspace
* super+right-arrow: tile window with focus RHS workspace
* super+H: hide (minimize) window
* super+click: raise window without passing click to window
* super+shift+page-up: move window with focus up one workspace
* super+shift+page-down: move window with focus down one workspace
* ctrl+shift+alt+r toggle screen recording (store in ~/Videos)
* ctrl+alt+up-arrow: move up one workspace
* ctrl+alt+down-arrow: move up one workspace
* super-click: raise window
* click titlebar: raise window
* middle-click titlebar: drop window below all other windows
* double-click titlebar: toggle maximize window

## Paradigms

* No desktop icons
* Desktop acts as a window manager
* The Dash acts like a program manager
  * right-click to
    * select among running instances
    * launch new window
    * maybe also launch new instance
  * shared menu top of screen - shows application for window with focus
  * no longer possible to configure to "launch new instance"
    * as of 3.38.4
    * making work like MacOS is a misfeature

## Gnome-Terminal

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

## Gnome-Shell-Extensions

* Arch Linux
  * Use Pacman if extension is in main Arch repos (like gpaste)
  * Turn on via Gnome-Tweak
* Arch Linux using AUR
  * Go to [AUR](https://aur.archlinux.org/)
  * Search on gnome-shell-extension
  * Create a place to build the extension

    ```
       $ mkdir -p ~/build/AUR/gnome-extensions/
    ```

  * Example: Install gnome-shell-extension-extensions

    ```
       $ cd ~/build/AUR/gnome-extensions/
       $ git clone https://aur.archlinux.org/gnome-shell-extension-extensions-git.git
       $ cd gnome-shell-extension-extensions-git
       $ makepkg -sri
    ```

  * This installs Extensions for all users
  * After re-logging in, use gnome-tweaks to activate Extensions
  * Extensions provides a convenient way to activate/deactivate other extensions
    * Extensions app has an option to install Gnome extentions
      * This just takes you to the Gnome extensions page (AppImages)
      * Does not auto install from AUR.
  * Turn on via Gnome-Tweak

## Download extension from [https://extensions.gnome.org/](https://extensions.gnome.org/)

* Extract into `~/.local/share/gnome-shell/extensions`
* Turn on via Gnome-Tweak
* Will be available only to that one user

## Freedesktop.org XDG Directory Specification

Gnome3 follows freedesktop.org desktop specifications (Gnome3 "upstream?").
Here are the environment variables and their defaults used by Gnome3

* `$XDG_CONFIG_HOME`
  * directory where user specific configuration files should be stored.
  * default value: `$HOME/.config`
* `$XDG_CONFIG_DIRS`
  * search path for configuration files
  * searched after `$XDG_CONFIG_HOME`
  * default value: `/etc/xdg`
* `$XDG_DATA_HOME`
  * directory where user specific data files should be stored
  * default value: `$HOME/.local/share`
* `$XDG_DATA_DIRS`
  * search path for data files
  * searched after `$XDG_DATA_HOME`
  * default value: `/usr/local/share/:/usr/share/`
* `$XDG_RUNTIME_DIR`
  * directory where user-specific non-essential runtime files stored
  * also other file objects such as sockets, named pipes, ...
  * directory MUST be owned by the user and MUST have file permissions 0700
  * on Arch Linux:`/run/user/<uid>`

As of Gnome-shell 3.38.4, I have noticed that GNOME3 does non-interactively
source ~/.profile profile.
