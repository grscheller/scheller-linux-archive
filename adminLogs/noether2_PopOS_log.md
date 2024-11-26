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
  * NVIDIA ISO (godel2, )

Verified checksums.

... see notebook notes

From gauss17 backups:

* moved fish files into ~/.config/fish
* moved previous SSH configs into ~/.ssh

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

## 11-26-2024:

Since Pop!OS bypasses a login shell, every time I open a new terminal,
I spawn a new SSH Agent. Need to fix this.

