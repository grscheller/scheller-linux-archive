# System Admin Log: noether2

## Purpose

To rebuild my gauss17 Linux laptop using Pop!OS in preparation to
configure my new work computer, godel2, for AI software development.

Named after mathematician Emmy Noether's two dynamical systems
symmetry <-> conserved quantity theorems.

## 2024-11-24:

Have not been able to configure systemd-boot for for an Arch reinstall
on gauss17. Both with fdisk and parted, not able to get UEFI firmware to
recognize the drive the bootloader is installed on as an EFI drive.

I don't remember ever going through this pain before. The thumbdrive
itself was configured with Grub.

* Maybe Pacman "magic" was done to properly update partition on update?
* Maybe my 8 year old UEFI firmware is too out-of-date?

## 2024-11-24:

Next step would have been to use Grub.

Instead decided to go with PoP!OS 24.04 Alpha III release

* was waiting for the actual release
* downloaded both
  * Intel/AMD ISO (gauss17 -> noether2)
  * NVIDIA ISO (godel2, euler7 -> hamilton4)

Verified checksums.

Followed GUI installation. Reformatted everything expect `/extra`.

```bash
    $ lsblk
    NAME          MAJ:MIN RM   SIZE RO TYPE  MOUNTPOINTS
    sda             8:0    0 119.2G  0 disk
    ├─sda1          8:1    0   1.9G  0 part  /boot/efi
    └─sda2          8:2    0 117.3G  0 part  /
    sdb             8:16   0 931.5G  0 disk
    ├─sdb1          8:17   0   512M  0 part
    ├─sdb2          8:18   0  63.5G  0 part  /extra
    ├─sdb3          8:19   0    16G  0 part
    │ └─cryptswap 252:0    0    16G  0 crypt [SWAP]
    └─sdb4          8:20   0 851.5G  0 part  /home
    zram0         251:0    0  15.5G  0 disk  [SWAP]
```

From gauss17 backups:

* moved fish files into `~/.config/fish`
* moved previous SSH configs into `~/.ssh`
* eventually moved all personal stuff into `/home/grs/`

First tweaks after install process.

* No stty
  * installed coreutils
  * font in virtual consoles way too big
* Installed non-nvidia version, but drives seem Optimus aware
  * can offload some processing to NVIDIA card
* COSMIC Store GUI much better designed
  * some items can be either System or Flathub installs
  * installed Flathub version Gnome Calculator
* Installed Nomacs image viewer via `apt`
* Settings App adjustments
  * chose Dark mode as default
  * active window hint color: #05676E
  * active window hint size: 5
  * gap around tiled windows: 6
  * swapped CAPS-LOCK now extra ESC keys
    * will need to change muscle memory

## 2024-11-25:

Need to get GIT working.

First install sshd server, then bring over GIT configuration from
euler7.

```bash
    $ sudo apt install openssh-server
    ...

    $ systemctl status ssh.service
    ● ssh.service - OpenBSD Secure Shell server
         Loaded: loaded (/usr/lib/systemd/system/ssh.service; disabled; preset: enabled)
         Active: active (running) since Mon 2024-11-25 12:14:16 MST; 1h 28min ago
    TriggeredBy: ● ssh.socket
           Docs: man:sshd(8)
                 man:sshd_config(5)
        Process: 2666 ExecStartPre=/usr/sbin/sshd -t (code=exited, status=0/SUCCESS)
       Main PID: 2667 (sshd)
          Tasks: 1 (limit: 18950)
         Memory: 3.4M (peak: 4.1M)
            CPU: 67ms
         CGroup: /system.slice/ssh.service
                 └─2667 "sshd: /usr/sbin/sshd -D [listener] 0 of 10-100 startups"

    $ scp -r -P XXXXX grs@10.54.4.125:.config/git .
    config
```

Where XXXXX is the port number I use on euler7 for ssh.

Note name change of service, now ssh.service, was sshd.service on Pop!OS
22.04 as it still is on Arch Linux.

GIT is configured to use Neovim.

```bash
    $ apt search '^neovim$'
    Sorting... Done
    Full Text Search... Done
    neovim/noble 0.9.5-6ubuntu2 amd64
      heavily refactored vim fork
```

But I am currently using Neovim v0.10.2 on Arch. Following what I did
for onepiece:

```bash
    $ sudo add-apt-repository ppa:neovim-ppa/unstable
    $ sudo apt update
    $ sudo apt upgrade
    $ sudo apt install neovim
    $ nvim --version
    NVIM v0.11.0-dev
    Build type: RelWithDebInfo
    LuaJIT 2.1.1703358377
    Run "nvim -V1 -v" for more info
```

Now I am even more bloody-edge than I was on Arch!

The apt upgrade also updated a lot of stuff.

## 11-25-2024:

Went and installed the rg and fd utilities,

```bash
    $ sudo apt install fdclone ripgrep
```
but `fdclone` was not what I wanted.

```bash
    $ sudo apt autoremove fdclone
    $ sudo apt install fd-find
    $ sudo ln -s ../lib/cargo/bin/fd fd
    $ ls -l /usr/bin/fd*
    lrwxrwxrwx 1 root root 19 Nov 25 20:59 /usr/bin/fd -> ../lib/cargo/bin/fd
    lrwxrwxrwx 1 root root 19 Dec 30  2023 /usr/bin/fdfind -> ../lib/cargo/bin/fd
```

What the hell is so important that it can retain the `fd` name?

```bash
    $ apt search fdclone
    Sorting... Done
    Full Text Search... Done
    fdclone/noble,now 3.01j-1 amd64 [residual-config]
      console-base lightweight file manager
```

IDIOTS!!! I would have renamed this thing to `fm`.

## 11-25-2024:

Finished cloning my GIT repos.

* `~/devel/dotfiles`
* `~/devel/scheller-linux-archive`
* `~/devel/web`
* `~/devel/notes/git-notes`
* `~/devel/notes/neovim-notes`
* `~/devel/grok/fpinScala3Stdlib`
* `~/devel/grok/grok-typescript`
* `~/devel/grok/grok-lua`
* `~/devel/pypi/fp`
* `~/devel/pypi/grscheller-pypi-namespace-docs`
* `~/devel/pypi/datastructures`
* `~/devel/pypi/circular-array`
* `~/devel/pypi/experimental`
* `~/devel/pypi/boring-math`
* `~/devel/courses/udacity/ai/courses-distributions`
* `~/devel/courses/udacity/ai/courses-pet-images`

## 11-25-2024:

Installed alacritty terminal emulator.

```bash
    $ apt search 'alacritty$'
    Sorting... Done
    Full Text Search... Done
    alacritty/noble-updates 0.13.2-1ubuntu1 amd64
      Fast, cross-platform, OpenGL terminal emulator
    $ sudo apt install alacritty
    $ sudo apt install fonts-roboto fonts-firacode
```

Was on alacritty v0.14.0+ on Arch Linux. Had to fix things in my
configs.

First, Pop!OS seems to have forked its fonts.

* See `https://github.com/pop-os/fonts`

Not sure if these are Nerd Fonts, but their names are Fira and
roboto-slab. I uninstalled fonts-roboto and fonts-firacode I installed
above and commented out these in Alacritty configs. With these,
alacritty looked horrible.

Also changed background to my usual Tokyo-at-night one. I loved the
default Pop!OS wallpaper, but it did not go well with Alacritty
transparency. Made the terminal look washed out.

## 11-25-2024:

Move my old Arch config to its own branch in my grscheller/dotfiles
repo. On the master branch I made my first stab at converting my
environmental scripts to Pop!OS.

Here goes nothing:

```bash
    $ dfInstall
```

Neovim has problems but at this point still the best editor I have.

The fonts don't appear to be nerd-fonts. Also `:checkhealth` indicates
Luarocks is missing.

## 11-26-2024:

Seems that the Cosmic-Terminal is a fork of Alacritty. I don't seem to
be able to tweak it though its menus to be as sharp and readable as my
highly configured Alacritty configuration. With transparency the Cosmic
Terminal just has a washed out feel to it. I think it is a "feature" and
not a "bug".

I suspect I will have better luck getting a Nerd-Font working with
Alacritty but may have to go back to manually installed fonts again.

Neovim is giving me a lot of deprecation warnings for its upcoming 1.0
release.

I will need to get my Python infrastructure working. Might fix some
of my Neovim problems.

## 11-26-2024:

Changed login shell for my user.

```bash
    $ sudo usermod -s /usr/bin/fish grs
```

## 11-26-2024:

Need to install Nerd fonts from
[here](https://www.nerdfonts.com/font-downloads). Previously FireCode
and RobotoMono Nerd fonts were installed on:

* onepiece:`~/.local/share/fonts` (better for a shared login)
* euler7:`/usr/local/share/fonts` (better for separate logins)

Since I am the only one who will use noether2, TODO:

```fish
    $ mkdir ~/.local/share/fonts
    $ ax RobotoMono.zip
    $ mv *.ttf ~/.local/share/fonts/
    $ ax FiraCode.zip
    $ mv *.ttf ~/.local/share/fonts/
```

## 11-27-2024:

Since Pop!OS bypasses a login shell, every time I open a new terminal,
I spawn a new SSH Agent. Need to fix this.

This worked on Arch with Sway because I did not use a Display Manager:

```fish
    # Ensure SSH key-agent running with your private keys
    if ! set -q SSH_AGENT_PID
       printf 'SSH '
       eval (ssh-agent -c)
       and ssh-add
    end
```

I tested the following on both Pop!OS and Arch.

```fish
    # Manage SSH key-agents
    function exit_handler --on-event fish_exit
       if status --is-login
           if set -q SSH_AGENT_PID
              kill -15 $SSH_AGENT_PID
           end
       end
    end

    if ! set -q SSH_AGENT_PID
       set -l ssh_flag 1
       set -l desktop_flag 0
       if set -q XDG_CURRENT_DESKTOP
          set desktop_flag 1
          if test -f /tmp/grs_ssh_desktop_env
             printf 'Last Desktop SSH '
             source /tmp/grs_ssh_desktop_env
             if ps -p $SSH_AGENT_PID > /dev/null
                set ssh_flag 0
             end
          end
       end
       if test "$ssh_flag" -eq 1
          set -l umask_orig $umask
          umask 0077
          printf 'SSH '
          if test "$desktop_flag" -eq 1
             eval (ssh-agent -c|tee /tmp/grs_ssh_desktop_env)
             and ssh-add
          else
             eval (ssh-agent -c)
             and ssh-add
          end
          umask $umask_orig
       end
    end
```

This is actually an improvement for both. Now the SSH Agent will clean
itself up after the login shell ends. The DE now shares a single SSH
Agent.

## 11-27-2024:

Manually install Nerd Fonts downloaded from the
[Nerd Fonts](https://www.nerdfonts.com/) website. As root,

```bash
    mkdir -p /usr/local/share/fonts/truetype/{firacode,robotomono}
    cd /usr/local/share/fonts/truetype/firacode
    unzip ~grs/catch/FireCode.zip
    cd ../robotomono
    unzip ~grs/catch/RobotoMono.zip/
    cd ../../..
    chmod -R root:root fonts
```

Neovim now looks better and gitsigns now working. Need to install
a clipboard manager. Already one installed, `xsel`. Does not play nice
with Firefox, but CTRL-SHIFT-V works to paste.

### Head scratcher:

COSMIC Terminal now supporting Nerd Fonts??? I think it is picking up on
my Alacritty configuration. Not sure what is going on. Need to change
default CTRL+T keybinding to Alacritty.

## 2024-11-28:

Set `Super+T` keybinding to `/usr/bin/alacritty`

* Removed "System" keybinding `Open a terminal`.
* Added "Custom" keybinding `Super+T` to `/usr/bin/alacritty`

I found the UI confusing for the "System" section. It did not give me
a choice to "edit" the keybinding. After removing it, I could have
either restored it or created the keybinding I did in the "Custom"
section.

## 2024-11-28:

Decided to configure pyenv, which is NOT in the Pop!OS repos.

Fish already sets `$PYENV_ROOT` to `~/.local/share/pyenv` which needs to
not exist for the curl install to work.

```fish
    $ curl https://pyenv.run | bash
```

Update pyenv itself.

```fish
    $ pyenv update
```

From the
[pyenv wiki](https://github.com/pyenv/pyenv/wiki#suggested-build-environment),
the suggested build environment for Ubuntu/Debian/Mint is

```fish
    sudo apt update; sudo apt install build-essential libssl-dev \
    zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl git \
    libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev \
    libffi-dev liblzma-dev
```

Install Python versions:

```fish
    $ pyenv install 3.12.7
    $ pyenv install 3.13.0
    $ pyenv versions
    * system (set by /home/grs/.local/share/pyenv/version)
      3.12.7
      3.13.0
```

Test Python out (happy path) - I manage my python virtual environments
with a fish script named `ve` instead of using pyenv to do such.

```fish
    $ pyenv shell 3.12.7
    $ ve dev
    $ ve -c -r
    $ pypath circulararray
    $ cd ~/devel/pypi/circular-array
    $ pytest tests/
```

## 2024-11-29:

Time to give Neovim some love. From feedback from `:checkhealth` I will
install some things.

```fish
    $ sudo apt install fswatch nodejs npm
    $ sudo npm install -g neovim
    $ sudo npm install -g tree-sitter-cli

    $ ls -l (digpath tree-sitter)
    lrwxrwxrwx 1 root root 42 Nov 29 10:45 /usr/local/bin/tree-sitter -> ../lib/node_modules/tree-sitter-cli/cli.js
    $ ls /usr/local/lib/node_modules/
    neovim  tree-sitter-cli
```

Treesitter problem now fixed, but LSP failing for Lua. Also,
`:checkhealth` giving massive deprecation warnings for many installed
plugins. Maybe I am a bit too "bleeding-edge."

```fish
    $ sudo apt remove neovim   # MISTAKE!
    $ sudo add-apt-repository -r ppa:neovim-ppa/unstable
    $ sudo apt install neovim
    ...
    The following packages have unmet dependencies:
    neovim : Depends: neovim-runtime (= 0.9.5-6ubuntu2) but 0.10.0~ubuntu1+git202411231006-c33ec2d7ce-971e32c878-b63bff4404~ubuntu24.04.1 is to be installed
             Recommends: python3-pynvim but it is not going to be installed
    E: Unable to correct problems, you have held broken packages.
    $ sudo apt autoremove neovim   # Fixed mistake.
    $ sudo apt install neovim
```

Good news: TS highlighting now works in Lua files!
Bad news: There is now another problem...

```
    Error detected while processing BufReadPre Autocommands for "*":
    lazydev.nvim requires Neovim >= 0.10
```
Lets try going to the PPA for the stable version.

```fish
    $ sudo apt autoremove neovim
    $ sudo add-apt-repository ppa:neovim-ppa/stable
    $ sudo apt install neovim
    $ nvim --version
    NVIM v0.9.5
    Build type: Release
    LuaJIT 2.1.1703358377
```

Oops... this version is "too stable."

```fish
    $ sudo apt autoremove neovim
    $ sudo add-apt-repository -r ppa:neovim-ppa/stable
    $ sudo add-apt-repository ppa:neovim-ppa/unstable
    $ sudo apt install neovim
    $ nvim --version
    NVIM v0.11.0-dev
    Build type: RelWithDebInfo
    LuaJIT 2.1.1703358377
```

Not sure what I fixed, but LSP now working for Lua. Maybe previously
I was not running it in a virtual environment?

Open Neovim issues:

* XSel may not be the right clipboard tool for Wayland
  * sometimes it works, sometimes not
* Surround plugin not working
  * was not working on Arch
  * version I use may be deprecated
  * surround plugins are a dime-a-dozen
* neoconf warns that neodev.nvim not installed
  * I had thought neodev was deprecated???

Addressing first one:

```fish
    $ sudo apt autoremove xsel
    $ sudo apt install wl-clipboard
```

2024-11-30:

Did much needed maintenance on on my Neovim configurations. Time to
install Pop!OS onto hamilton4. RIP euler7.

2024-12-06:

Having some trouble with godel2 negotiating with cards. Tried swapping
out NetworkManager with systemd-networkd. Will switch noether2 to
systemd-networkd.

Following
[this xmodulo.com article](https://www.xmodulo.com/switch-from-networkmanager-to-systemd-networkd.html).

```bash
    systemctl disable NetworkManager
    ...
    systemctl enable systemd-networkd
    ...
    systemctl enable systemd-resolved
    ...
```

Huh? No symlinks changed.

```fish
    $ systemctl status systemd-resolved
    ● systemd-resolved.service - Network Name Resolution
         Loaded: loaded (/usr/lib/systemd/system/systemd-resolved.service; enabled; preset: enabled)
         Active: active (running) since Fri 2024-12-06 12:17:27 MST; 5h 5min ago
           Docs: man:systemd-resolved.service(8)

                 man:org.freedesktop.resolve1(5)
                 https://www.freedesktop.org/wiki/Software/systemd/writing-network-configuration-managers
                 https://www.freedesktop.org/wiki/Software/systemd/writing-resolver-clients
       Main PID: 652 (systemd-resolve)
         Status: "Processing requests..."
          Tasks: 1 (limit: 18950)
         Memory: 5.6M (peak: 6.2M)
            CPU: 1.441s
         CGroup: /system.slice/systemd-resolved.service
                 └─652 /usr/lib/systemd/systemd-resolved
    
    Dec 06 12:17:27 noether2 systemd-resolved[652]: Positive Trust Anchors:
    Dec 06 12:17:27 noether2 systemd-resolved[652]: . IN DS 20326 8 2 e06d44b80b8f1d39a95c0b0d7c65d08458e880409bbc683457104237c7f8ec8d
    Dec 06 12:17:27 noether2 systemd-resolved[652]: Negative trust anchors: home.arpa 10.in-addr.arpa 16.172.in-addr.arpa 17.172.in-addr.arpa 18.172.in-addr.arpa 19.172.in-addr>
    Dec 06 12:17:27 noether2 systemd-resolved[652]: Using system hostname 'noether2'.
    Dec 06 12:17:27 noether2 systemd[1]: Started systemd-resolved.service - Network Name Resolution.
    Dec 06 12:17:37 noether2 systemd-resolved[652]: wlp2s0: Bus client set default route setting: yes
    Dec 06 12:17:37 noether2 systemd-resolved[652]: wlp2s0: Bus client set DNS server list to: 192.168.1.1
    Dec 06 12:17:37 noether2 systemd-resolved[652]: Using degraded feature set UDP instead of UDP+EDNS0 for DNS server 192.168.1.1.
    Dec 06 12:17:39 noether2 systemd-resolved[652]: wlp2s0: Bus client set DNS server list to: 192.168.1.1, fe80::9ec9:ebff:fe54:5f75
    Dec 06 12:17:42 noether2 systemd-resolved[652]: Using degraded feature set UDP instead of UDP+EDNS0 for DNS server fe80::9ec9:ebff:fe54:5f75%3.
```

Already running, using the router as its DNS upstream, but in some sort
of "degraded" UDP mode.

```bash
    ls -l /etc/resolv.conf
    lrwxrwxrwx 1 root root 39 Nov 24 18:14 /etc/resolv.conf -> ../run/systemd/resolve/stub-resolv.conf
```

But the article says to instead do

```fish
   $ sudo ln -s /run/systemd/resolve/resolv.conf /etc/resolv.conf
```

This is triggering some old memories, been down this rabbit-hole before.
Seems that the article has you bypass the name server running locally in
favor for, in my case at home, the router's cached DNS service.

Will leave link as it is. May have to fix on godel2.

Create just one interface file, `/etc/systemd/network/20-dhcp.network`.

```
    [Match]
    Name=enp3s01

    [Network]
    DHCP=yes
```

Note: could have used glob matches like "enp3*" instead

Turned WiFi off, power off, attach ethernet cable, and see where we are.
Will need to figure out an "easy" WiFi configuration solution. Maybe
write a script around `iwctl`?

Interesting...

```
   $ ip addr
   1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000
       link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
       inet 127.0.0.1/8 scope host lo
          valid_lft forever preferred_lft forever
       inet6 ::1/128 scope host noprefixroute
          valid_lft forever preferred_lft forever
   2: enp3s0f1: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UP group default qlen 1000
       link/ether f0:76:1c:cc:c1:05 brd ff:ff:ff:ff:ff:ff
       inet6 fe80::f276:1cff:fecc:c105/64 scope link
          valid_lft forever preferred_lft forever
   3: wlp2s0: <BROADCAST,MULTICAST> mtu 1500 qdisc noop state DOWN group default qlen 1000
       link/ether ac:e0:10:8a:3d:85 brd ff:ff:ff:ff:ff:ff
```

I only got a non-routeable IPv6 address. Also the desktop network "app"
has reduced functionality, as I would expect.

After switching the `/etc/resolv.conf` from `stub-resolv.conf` to
`resolv.conf` and rebooted, network very responsive. But I am totally
bypassing the local systemd-resolved DNS server. The real fix is to
properly configure systemd-resolved.

Aside: Seems that search engines do not provide as much insider system
and network admin content as they did a few years back.

## 2025-02-06:

Installed Cisco secure connect VPN client as I did for hamilton4.

Reused download I did for hamilton4. I suspect CISCO only let me do this
from the CCTI network.

## 2025-03-08:

Had to globally update tree-sitter node package.

```fish
    $ tree-sitter --version
    tree-sitter 0.24.4 (fc8c1863e2e5724a0c40bb6e6cfc8631bfe5908b)
    $ sudo npm uninstall -g tree-sitter-cli
    $ sudo npm install -g tree-sitter-cli
    $ tree-sitter --version
    tree-sitter 0.25.3 (2a835ee029dca1c325e6f1c01dbce40396f6123e)
    $ digpath tree-sitter
    /usr/local/bin/tree-sitter
    $ realpath (digpath tree-sitter)
    /usr/local/lib/node_modules/tree-sitter-cli/cli.js
```

Neovim had been giving me a strange error:

```
    Warning: your package.json's `tree-sitter` field has been automatically migrated to the new `tree-sitter.json` config file
    For more information, visit https://tree-sitter.github.io/tree-sitter/creating-parsers
    The --no-bindings flag is no longer used and will be removed in v0.25.0thread 'main' panicked at cli/generate/src/render.rs:1703:5:
    This version of Tree-sitter can only generate parsers with ABI version 13 - 14, not 15
    note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

Neovim error message went away after upgrading tree-sitter

## 2025-03-14:

Migrating nvim config changes done on godel2 and hamilton4.

#### Install Coursier onto noether2.

Coursier is the Scala application and artifact manager.
It can install Scala applications and setup your Scala development environment.
It can also download and cache artifacts from the web.

```fish
    $ cd ~/.local/bin
    $ curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
    $ file cs
    cs: ELF 64-bit LSB pie executable, x86-64, version 1 (SYSV), dynamically
    linked, interpreter /lib64/ld-linux-x86-64.so.2,
    BuildID[sha1]=df2579d460caf49798b5c6b05693d1693938808a, for GNU/Linux
    3.2.0, with debug_info, not stripped0
```

#### Coursier commands:

* install application commands:
  * cs install    Install an application from its descriptor.
  * cs list       List all currently installed applications.
  * cs setup      Setup a "machine" for Scala development.
  * cs uninstall  Uninstall one or more applications.
  * cs update     Update one or more applications.
* application channel commands:
  * channel  Manage additional channels, used by coursier to resolve application descriptors.
  * search   Search application names from known channels.
* java commands:
  * java       Manage installed JVMs and run java.
  * java-home  Print the home directory of a particular JVM.
* launcher commands:
  * bootstrap  Create a binary launcher from a dependency or an application descriptor.
  * launch     Launch an application from a dependency or an application descriptor.
* resolution commands:
  * fetch    Transitively fetch the JARs of one or more dependencies or an application.
  * resolve  Resolve and print the transitive dependencies of one or more dependencies or an application.
* other commands:
  * about    Print details about the current machine and coursier launcher.
  * version  Prints the coursier version

#### Setup user environment

Setup my Linux environment for Scala development on noether2.

```fish
    $ cs setup
    Checking if a JVM is installed
    Found a JVM installed under /usr/lib/jvm/java-21-openjdk-amd64.
    
    Checking if ~/.local/share/coursier/bin is in PATH
      Should we add ~/.local/share/coursier/bin to your PATH via ~/.config/fish/config.fish? [Y/n] n
    
    Checking if the standard Scala applications are installed
      Installed ammonite
      Installed cs
      Installed coursier
      Installed scala
      Installed scalac
      Installed scala-cli
      Installed sbt
      Installed sbtn
      Installed scalafmt
```
Let's see what just got installed,

```fish
    $ ls ~/.local/share/coursier/bin
    amm  coursier  cs  sbt  sbtn  scala  scalac  scala-cli  scalafmt
    $ ~/.local/bin/cs version
    2.1.25-M3
    $ ~/.local/share/coursier/bin/cs version
    2.1.25-M3
    $ cd ~/.local/share/coursier/bin
    $ file *
    amm:       a /usr/bin/env sh script executable (Zip archive)
    coursier:  a /usr/bin/env sh script executable (Zip archive)
    cs:        a /usr/bin/env sh script executable (Zip archive)
    sbt:       a /usr/bin/env sh script executable (Zip archive)
    sbtn:      a /usr/bin/env sh script executable (Zip archive)
    scala:     a /usr/bin/env sh script executable (Zip archive)
    scala-cli: a /usr/bin/env sh script executable (Zip archive)
    scalac:    a /usr/bin/env sh script executable (Zip archive)
    scalafmt:  a /usr/bin/env sh script executable (Zip archive)
```

OK, I have really fallen behind in the curve regarding Scala
development. Need to research how to do Scala development in 2025.

Next configure my fish environment.

* put `~/.local/share/coursier/bin` in path
* using JDK version 21
* removed `~/.local/bin/cs`

Surprise! I was already putting `~/.local/share/coursier/bin` in my
path, I don't remember setting this up before, must have been following
monkey-see-monkey-do directions.

#### Try using Metals

Tried using Metals without updating its configuration. Tooling seemed to
function, but LSP session devolved horribly. Logs files were put here:
`.metals/metals.log` relative to root of build directory. Removed
timestamps for brevity and wrapped lines for clarity.

```
Started: Metals version 1.5.1 in folders \
  '/home/grs/devel/scheller-linux-archive/grok/Scala/scalaImplicits' \
  for client Neovim 0.11.0-dev.
no build target found for \ 
  /home/grs/<snip>/src/main/scala/ScalaImplicits.scala. \
  Using presentation compiler with \
  project's scala-library version: 3.3.4
sbt 1.7.1 found for workspace.
[info] welcome to sbt 1.7.1 (Ubuntu Java 21.0.6)
error:
  bad constant pool index: 0 at pos: 48454
     while compiling: <no file>
        during phase: globalPhase=<no phase>, enteringPhase=<some phase>
     library version: version 2.12.16
    compiler version: version 2.12.16
  reconstructed args: -classpath /home/grs/.sbt/boot/scala-2.12.16/lib/scala-library.jar -Yrangepos
```

#### TODO going forward

* go to `scalameta/nvim-metals` and read plugins install & config info
* figure out what I need to do to configure Metals LSP server
* educate myself on
  * how to set up a Scala project using dev env installed by `cs setup`
  * the differences between the SBT and Bloom build tools
  * how to build something from the cmdline
  * learn best practices on how to use Scala for new projects in 2025.

I maybe a bit "old school" but if I maintain an old project, I'd rather
maintain the old tooling separately from the current tooling. This
business of having new tooling install old tooling may make things
"easy" but not "simple." If I dust off an old CI/CD pipeline, I don't
want to have to upgrade it to use modern tooling.

## 2025-04-12:

Noticed system a bit sluggish today. Also a core file in root directory.
When I investigated it, system became unusable. System would not reboot.

I booted off old flash drive with PopOS LTS stable, downloaded latest Alpha
ISO and installed it to to the flashdrive. Booted off the flshdrive and did
a fresh install. Reformatted everything except the home directory.

Reinstalled the OS and recreated my user in the process.

## 2025-04-18:

More or less got back to where I was.

Current fish versiona bit prehistoric, version 3.7.1. 

```
   $ sudo add-apt-repository ppa:fish-shell-shell/release-4
   $ sudo apt upgrade  # held back fish
   $ sudo apt autoremove
   & sudo apt full-upgrade
```

And then did a reboot.

Now on fish version 4.0.1. Hey, that's Rust!

## 2025-04-29:

Node and tree-sitter on POP!OS 24.04 way too old. I did not want to
install locally and I can find no PPA's for these. Installed tarballs
under /usr/local/share for these and creating symlinks from
/usr/local/bin.

```
    $ pwd
    /usr/local/bin
    $ ll
    total 0
    lrwxrwxrwx 1 root root 41 Apr 20 13:27 tree-sitter -> ../share/grs/tree-sitter-v0.25.1-linux-64
    lrwxrwxrwx 1 root root 52 Apr 20 14:09 corepack -> ../share/grs/node-v22.14.0-linux-x64/lib/corepack.js
    lrwxrwxrwx 1 root root 51 Apr 20 14:10 npx -> ../share/grs/node-v22.14.0-linux-x64/lib/npx-cli.js
    lrwxrwxrwx 1 root root 51 Apr 20 14:10 npm -> ../share/grs/node-v22.14.0-linux-x64/lib/npm-cli.js
    lrwxrwxrwx 1 root root 45 Apr 20 14:11 node -> ../share/grs/node-v22.14.0-linux-x64/bin/node
```
 Now I am wondering what I did for hamilton4 and godel2.
