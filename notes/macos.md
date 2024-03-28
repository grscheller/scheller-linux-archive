# MacOS Notes

Or how to work around the horrible mess Apple put on top of Darwin when
I have to use an Apple office productivity appliance as a Unix
workstation at work.

## Disclaimer

I will not personally use Apple products due to

* Use of forced prison labor by Apple's "partners" in China
* The abuse of workers in foreign manufacturing facilities
* Overcharging for their products yet
  * lack of US domestic manufacturing
  * using foreign labor to "cut costs" but not passing on savings
  * overpaying for many US domestic "bullshit" jobs
* Lack of interoperability with non-Apple products
* Lack of upgradability of their products
  * Voiding warranties if you open up your personal property
  * Soldering in hard drives
  * Making upgrading memory problematic for the average user
  * Dropping support for OS upgrades on older non-upgradable hardware
* Avoiding the use of modern FOSS licenses
  * resulting in using old crufty versions of POSIX tooling
  * requiring the use of third party tooling for software development
  * not returning to the FOSS community yet being a Berkeley derived Unix
* Lack of an adequate system package manager
* Wanting to keep their user base "dumb"
  * making software development more difficult than it needs to be
  * forcing users to adapt to bad choices made for a non-configurable UI
* Default software development tools provided too old, yet
  * not providing an adequate system package manager to upgrade them
  * forcing the use of a 3rd party package managers like
    * Homebrew
    * SDKMAN!
    * Nix

## Third Party Package Managers

In the old "Unix Wars" days, software developers had a saying, "Sun
Solaris is not Unix until you install a C compiler." For years I have
been running MacOS in "developer mode" to avoid it rejecting the
software I write. The "full stack" web developers I work with can only
program with a Mac running vscode. Most of them seem to have never
written anything that didn't run in the cloud or a browser. Asking them
how to software develop on a Mac is useless. This is usually what you
get coming out of software bootcamps, and increasingly what I am seeing
coming out of colleges. These monkey trained victims remind me of the
CGI Perl and VBA developers I have worked with in the past.

### Homebrew

#### How to Install Homebrew GUI Tools ("Apps")

Here is an example of how to install a GUI tool and not having to be in
developer mode to use it.

```
   $ brew install --cask --no-quarantine alacritty
```

Since Alacritty is a GUI app (icon in the Applications directory),
this means it’s packaged as a Homebrew 'cask', and has to be installed
with the --cask flag.

## MacOS Soyboy Administration

### To reboot MacOS from Apple keyboard:

* Ctrl + Command + Power

### Reimaging a Mac mini:

1. Make sure Apple keyboard & mouse are plugged into computer and turned on.
2. Make sure you have Ethernet cable plugged with connectivity to Internet.
3. Power off system.
4. Hold down key combo Opt-Cmd-R while rebooting.
5. Once booted into "recovery app", unplug mouse & keyboard.
6. Wipe HD with Gisk Utility.
6. Select Reinstall macOS.
7. Fumble through idiot instructions app provides.
8. Upgrade to latest compatible MacOS.
