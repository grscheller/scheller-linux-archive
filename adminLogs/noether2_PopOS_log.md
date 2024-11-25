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
  * NVIDIA ISO (godel2)

Verified checksums.

... see notebook notes

From gauss17 backups:

* moved fish files into ~/.config/fish
* moved previous SSH configs into ~/.ssh

## 2024-11-25:

Need to get GIT working.

First install sshd server, then bring over GIT configuration from
euler7.

```
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

```
   $ apt search '^neovim$'
   Sorting... Done
   Full Text Search... Done
   neovim/noble 0.9.5-6ubuntu2 amd64
     heavily refactored vim fork
```
But I am currently using Neovim v0.10.2 on Arch. Following what I did
for onepiece:

```
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
