# System Admin Log: hamilton4

## Purpose

To rebuild my euler7 Arch Linux laptop using Pop!OS. Arch Linux has
served me well and I have learned much, but right now Pop!OS serves
better serving my needs on the hardware I have access to and my time
restraints.

Named after mathematician William Rowan Hamilton's discovery of the
Quaternions. The Quanterions form a four-dimensional associative normed
division algebra over the real numbers, and therefore a ring, also
a division ring and a domain. It was the first non-commutative division
algebra to be discovered. Basically, except for not being commutative,
it behaves like a Field. 

2024-11-30:

I needed to get my work information off of euler7. I used SSH for that.

Currently on euler7:

```
$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINTS
nvme0n1     259:0    0 476.9G  0 disk
├─nvme0n1p1 259:1    0   512M  0 part /boot
├─nvme0n1p2 259:2    0  16.1G  0 part [SWAP]
└─nvme0n1p3 259:3    0 460.3G  0 part /
nvme1n1     259:4    0  27.3G  0 disk
└─nvme1n1p1 259:5    0  27.2G  0 part /home
```

I will carve it up differently. When the system was Windows the two nvme
drives were RAID'ed together. I think the smaller drive is an optical
drive. Rough estimate:

* nvme0n1p1   /boot/efi     2G      0.5%
* nvme0n1p2   swap         20G        4%
* nvme0n1p3   /           100G       21%
* nvme0n1p4   /home       359G       74%
* nvme1n1p1   /var         27G      100%


I'll put /var on the smaller drive. On the big drive I'll put `swap`,
`/`, `/home`, and `/boot/efi` on it. I'll put `/root` and `/home` on
the larger drive.

F12 puts you into the UEFI. When the system was Maxwell4 I set up the
boot order so that I could boot off the USB stick without going into
UEFI firmware. Had to fix boot order.

Went through thru install but it never asked me to set up wifi. Sat at
a screen forever with a reboot choice and a grayed out shutdown choice.
What to do? After waiting about 10 minutes I pulled the USB and pressed
reboot. System boot failed. I rebooted with thumb drive in but system
came up on the hard drive. Boot failed, but I was left in an emergency
shell.

I thought maybe the Optimus hybrid Intel-NVIDIA setup had a problem. I did
a `journalctl -xb` but the only NVIDIA errors were just the usual
"taints the kernel" crap. Did see it try to access the USB Flash Drive.
Got a taint message regarding the "sd" driver too? Only the nvme0n1
partitions mounted, read only.

This stupid computer HAS NO ETHERNET PORT! Went to run iwctl, guess
what, not installed. I will go into the UEFI firmware again and try to
boot off the flash drive.

Rebooted. The boot order was changed back to booting off the hard drive.
I was not expecting this. Did the Pop!OS installer or the UEFI firmware
do this?

Install asked me if I wanted to do a fresh install or recover the last
install. I did the latter. Lets see if I can get ModemManager to connect
me to WiFi. 

Yes!

First steps I did after getting WiFi access were,

```
   $ sudo apt update
   $ sudo apt upgrade
   $ sudo apt install iwd neofetch
```

Neofetch and Cosmic DE tell me I have:

* Intel i&-105100 (8) 4.90 GHz CPU
* NVIDIA GeForce MX250
* Intel CometLake-UG72 (UHD Graphics)
* 1561 MiB Memory

2024-12-01:

The install did not set up mounting the `/var` partition.

```
$ lsblk -f
NAME          FSTYPE FSVER LABEL     UUID                                 FSAVAIL FSUSE% MOUNTPOINTS
zram0                                                                                    [SWAP]
nvme0n1
├─nvme0n1p1   vfat   FAT32           0365-F73A                               1.9G     4% /boot/efi
├─nvme0n1p2   swap   1               72a74e32-7b68-457f-8a53-8e33ee7c14ac
│ └─cryptswap swap   1     cryptswap 9fbca8e4-b371-4820-bdcd-3f5e51ac357f                [SWAP]
├─nvme0n1p3   ext4   1.0             48746d86-adf0-4203-a5a3-ab0a1e4c8196     85G     8% /
└─nvme0n1p4   ext4   1.0             14193911-92b7-4926-ae37-f4f83fa49b37  334.1G     0% /home
nvme1n1
└─nvme1n1p1   ext4   1.0             affc2f9f-43fa-4299-ade5-284a3a1747d8
```

Will need to edit `/etc/fstab`. Its contents are:

```
   # /etc/fstab: static file system information.
   #
   # Use 'blkid' to print the universally unique identifier for a
   # device; this may be used with UUID= as a more robust way to name devices
   # that works even if disks are added and removed. See fstab(5).
   #
   # <file system>  <mount point>  <type>  <options>  <dump>  <pass>
   PARTUUID=4f484256-99ea-424c-b3cd-b3ba044e3410  /boot/efi  vfat  umask=0077  0  0
   /dev/mapper/cryptswap  none  swap  defaults  0  0
   UUID=48746d86-adf0-4203-a5a3-ab0a1e4c8196  /  ext4  noatime,errors=remount-ro  0  1
   UUID=14193911-92b7-4926-ae37-f4f83fa49b37  /home  ext4  noatime,errors=remount-ro  0  0
```

Double check UUID label.

```
   # blkid
   /dev/nvme0n1p3: UUID="48746d86-adf0-4203-a5a3-ab0a1e4c8196" BLOCK_SIZE="4096" TYPE="ext4" PARTUUID="2efa19fe-9dcb-4df2-86a7-00798225ef75"
   /dev/nvme0n1p1: UUID="0365-F73A" BLOCK_SIZE="512" TYPE="vfat" PARTUUID="4f484256-99ea-424c-b3cd-b3ba044e3410"
   /dev/nvme0n1p4: UUID="14193911-92b7-4926-ae37-f4f83fa49b37" BLOCK_SIZE="4096" TYPE="ext4" PARTUUID="32163870-0182-40a9-ae8f-18aa844c07f4"
   /dev/nvme0n1p2: UUID="72a74e32-7b68-457f-8a53-8e33ee7c14ac" TYPE="swap" PARTUUID="d9973efb-ab99-4898-9398-6d3852b2d2be"
   /dev/mapper/cryptswap: LABEL="cryptswap" UUID="9fbca8e4-b371-4820-bdcd-3f5e51ac357f" TYPE="swap"
   /dev/nvme1n1p1: UUID="affc2f9f-43fa-4299-ade5-284a3a1747d8" BLOCK_SIZE="4096" TYPE="ext4" PARTUUID="4d9f2b91-7bd9-46ee-8a20-41cca6d09b18"
   /dev/zram0: UUID="8fa379eb-ea3a-4346-9909-709e7c55f8db" TYPE="swap"cWaffc2f9f-43fa-4299-ade5-284a3a1747d8
```

Added a line to mount `/var` in `/etc/fstab`. Also set the last field
for `/home` and `\var` to `2` so that both are checked when file system
checks need to be done.

```
UUID=14193911-92b7-4926-ae37-f4f83fa49b37  /home  ext4  noatime,errors=remount-ro  0  2
UUID=affc2f9f-43fa-4299-ade5-284a3a1747d8  /var  ext4  noatime,errors=remount-ro  0  2
```
Chicken or egg time.

Here is what I did next, a bit risky but I was wasting too much time
trying to figure out the modern equivalent of a "single user" run level.
I think it is "emergency repair?" or something like that. The forth
command below was based on a warning Pop!OS (Systemd?) gave me after the
mount command.

```
   # mv /var /var/old
   # mkdir /var
   # mount /var
   # systemctl daemon-reload
   # mv /var_old/* /var/
   mv: inter-device move failed: '/var_old/cache' to '/var/cache'; unable to remove target: Directory not empty
   mv: inter-device move failed: '/var_old/lib' to '/var/lib'; unable to remove target: Directory not empty
   mv: inter-device move failed: '/var_old/log' to '/var/log'; unable to remove target: Directory not empty
   mv: inter-device move failed: '/var_old/spool' to '/var/spool'; unable to remove target: Directory not empty
   mv: inter-device move failed: '/var_old/tmp' to '/var/tmp'; unable to remove target: Directory not empty
   # reboot
```

I forgot that since `/` and `/var` are now on different partitions
I would be ripping i-nodes out from under system processes. Bad boy.
After the reboot everything seems to be working fine.

## 2024-12-01:

I am repeating much of the "user environment" configurations I did on
noether2. I won't repeat them here, but some of the system wide ones
I will when I configure godel2.

## 2024-12-01:

Install some more utilities

```
   $ sudo apt install coreutils
   $ sudo apt install fd-find ripgrep
   $ sudo apt install alacritty
   $ sudo apt install nomacs
```

Now install a functional Neovim:

```
   $ sudo add-apt-repository ppa:neovim-ppa/unstable
   $ sudo apt install neovim wl-clipboard
   $ nvim --version
   Run "nvim -V1 -v" for more info
   NVIM v0.11.0-dev
   Build type: RelWithDebInfo
   LuaJIT 2.1.1703358377
```

Installed Nerd fonts `firacode` and `robotomono`. Used the Nerd Fonts
I downloaded and installed on noether2.

```
   # mkdir -p /usr/local/share/fonts/truetype/{firacode,robotomono}
   # cd /usr/local/share/fonts/truetype/firacode
   # unzip ~grs/catch/FireCode.zip
   # cd ../robotomono
   # unzip ~grs/catch/RobotoMono.zip/
```

Need to install my shell environments to get my ssh keys working, and
for my own sanity.

```
   $ cd
   $ mkdir devel
   $ cd devel
   $ git clone https://github.com/grscheller/dotfiles
   $ cd dotfiles/bin
   $ ./homeInstall
   $ bash
   $ ls -l /tmp
   total 60
   drwx------ 2 grs  grs  4096 Dec  1 17:37 ssh-Yb9MqTePOhTA
   ...
```

Now I have an ssh key agent running.

Went ahead and did an `dfInstall`. I forgot that this triggers nvim to
generate a spelling file. Neovim more or less configured itself. I ran
lazy and mason manually, but was still missing Treesitter.

Might as well finish getting Neovim working.

```
   $ sudo apt install fswatch nodejs npm
   $ sudo npm install -g neovim
   $ sudo npm install -g tree-sitter-cli
```

Except for the lack of Nerd Fonts, Neovim seems to be functioning OK.
When nvim is run in an alacritty terminal, Nerd fonts work great.

If I launch the cosmic-term from alacritty, nerd fonts work great there
too. OK, font problem happens when I have old cosmic-term terminals
sitting around from before I installed the nerd fonts.

## 2024-12-01:

Lets get fish installed.

```
   $ sudo apt install fish
   $ sudo usermod -s /usr/bin/fish grs
   $ 
```

Sorted out some issues with ssh keys and got my GIT infrastructure
working. Cloned all my GitHub repos.

Got pyenv working, just repeated what I did for norther2.

## 2024-12-01:

Nvidia related service failing on boot:

```
   $ systemctl list-units|grep -i failed
   ● nvidia-powerd.service                                                                                                                                               loaded failed failed    nvidia-powerd service
```
Maybe `nvidia-powerd` does not support hamilton4 older Optimus hybrid
graphics. I will try and turn it off. From askUbuntu.com,

```
   $ sudo systemctl disable nvidia-powerd.service
   $ Removed "/etc/systemd/system/multi-user.target.wants/nvidia-powerd.service".
   $ reboot
```

## 2024-12-04:

Getting Avahi installed and hopefully auto configured.

```
   $ sudo apt-get install avahi-daemon avahi-discover avahi-utils libnss-mdns mdns-scan
```

Also, `nvidia-powerd` is failing on godel2, so I will re-enable it on
hamilton4.

```
   $ systemctl enable nvidia-powerd
```

Reboot.