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

```
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

IDIOTS!!! I would have renamed it to `fm`.

## 11-25-2024:

Finished cloning my GIT repos.

```bash
   $ cd ~/devel
   $ fdgit

   notes/git-notes:
      ## main...origin/main

   web:
      ## main...origin/main

   notes/neovim-notes:
      ## master...origin/master

   scheller-linux-archive:
      ## master...origin/master
    M adminLogs/noether2_PopOS_log.md
      adminLogs/noether2_PopOS_log.md | 40 ++++++++++++++++++++++++++++++++++++++++
      1 file changed, 40 insertions(+)

   dotfiles:
      ## master...origin/master

   pypi/fp:
      ## main...origin/main

   grok/fpinScala3Stdlib:
      ## main...origin/main

   grok/grok-typescript:
      ## main...origin/main

   grok/grok-lua:
      ## main...origin/main

   pypi/grscheller-pypi-namespace-docs:
      ## main...origin/main

   pypi/datastructures:
      ## main...origin/main

   pypi/circular-array:
      ## main...origin/main

   pypi/experimental:
      ## main...origin/main

   pypi/boring-math:
      ## main...origin/main

   courses/udacity/ai/courses-distributions:
      ## main...origin/main

   courses/udacity/ai/courses-pet-images:
      ## main...origin/main
```

## 11-25-2024:

Installed alacritty terminal emulator.

```
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

* See: https://github.com/pop-os/fonts

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

```
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

```
   $ sudo usermod -s /usr/bin/fish grs
```

## 11-26-2024:

Need to install Nerd fonts from
[here](https://www.nerdfonts.com/font-downloads). Previously FireCode
and RobotoMono Nerd fonts were installed on:

* onepiece:`~/.local/share/fonts` (better for a shared login)
* euler7:`/usr/local/share/fonts` (better for separate logins)

Since I am the only one who will use noether2, TODO:

```
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

```
   # Ensure SSH key-agent running with your private keys
   if ! set -q SSH_AGENT_PID
      printf 'SSH '
      eval (ssh-agent -c)
      and ssh-add
   end
```

I tested the following on both Pop!OS and Arch.

```
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

```
   # mkdir -p /usr/local/share/fonts/truetype/{firacode,robotomono}
   # cd /usr/local/share/fonts/truetype/firacode
   # unzip ~grs/catch/FireCode.zip
   # cd ../robotomono
   # unzip ~grs/catch/RobotoMono.zip/
   # cd ../../..ll
   # chmod -R root:root fonts     <- undid this later
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

```
   $ curl https://pyenv.run | bash
```

Update pyenv itself.

```
   $ pyenv update
```

From the
[pyenv wiki](https://github.com/pyenv/pyenv/wiki#suggested-build-environment),
the suggested build environment for Ubuntu/Debian/Mint is

```
   sudo apt update; sudo apt install build-essential libssl-dev \
   zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl git \
   libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev \
   libffi-dev liblzma-dev
```

Install Python versions:

```
   $ pyenv install 3.12.7
   $ pyenv install 3.13.0
   $ pyenv versions
   * system (set by /home/grs/.local/share/pyenv/version)
     3.12.7
     3.13.0
```

Test Python out (happy path) - I manage my python virtual environments
with a fish script named `ve` instead of using pyenv to do such.

```
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

```
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

```
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

```
   $ sudo apt autoremove neovim
   $ sudo add-apt-repository ppa:neovim-ppa/stable
   $ sudo apt install neovim
   $ nvim --version
   NVIM v0.9.5
   Build type: Release
   LuaJIT 2.1.1703358377
```

Oops... this version is "too stable."

```
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

```
   $ sudo apt autoremove xsel
   $ sudo apt install wl-clipboard
```

2024-11-30:

Did much needed maintenance on on my Neovim configurations. Time to
install onto hamilton4. RIP euler7.

