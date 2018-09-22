# Purpose: Gnome 3 configuration on CentOS 7

### GNOME Version:
```
   $ gnome-shell --version
   GNOME SHELL 3.25.4
```

### Initial impressions:
* Click-focus
* Top menu bar MAC like, whatever window has focus, gnome
  put a dropdown menu as last item on left side.  This probable
  alone screws up any hope of mouse focus being usable.
* Bottom menus bar acts as a toolbar program manager/window selection
  for current desktop.  Click far RHS to select desktop, four default
  desktops. 
* Drag mouse to left side top menu, acts as desktop progam
  manager/window selector on current desktop.  Mouse wheel or click
  vertical menu bar on RHS to select desktop.  Something like
  docky in a LHS menu bar.  (Overview mode)
* No functionality built into GUI to change focus model, info on
  internet using cmdline gconfigtool 5-6 years old.

### UTF-8 right out of the box:
```
   $ echo $LANG
   en_US.UTF-8
```

### GNOME Terminal configuration:
* Menu takes up minimal real estate, leave on.
* Edit -> Preferences -> Theme variant: Dark

### Manually turn on Ethernet Interface:
* Applet on right side top tool bar to left of sound applet

### More behaviors:
* super-tab to switch between windows on current desktop
* super-L to lock screen
* super (on key up) switches between the desktop and overview mode

### Highly configurable via gsettings:
* Enable sloopy focus (like Cinnamon & XFCE)
```
   $ gsettings set org.gnome.desktop.wm.preferences focus-mode sloppy
   $ gsettings set org.gnome.desktop.wm.preferences raise-on-click false
   $ gsettings set org.gnome.desktop.wm.preferences action-middle-click-titlebar lower
```
* Clear default icons from desktop
```
   $ gsettings set org.gnome.nautilus.desktop home-icon-visible false
   $ gsettings set org.gnome.nautilus.desktop trash-icon-visible false
```
