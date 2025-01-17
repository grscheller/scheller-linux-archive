# godel2 System Admin Log

## Purpose

To build a system for work geared to machine learning. System's name
will be godel2. Named after Kurt Godel's two incompleteness theorems.

## 2024-11-21:

### System Specs - Puget Systems

* Puget Workstation Ryzen X870E R120-L
  * AMD Ryzen 9 9900XNVIDIA GeForce RTX 4080 SUPER 16 GB
  * 1TB NVMe PCIe Gen4 M.2 SSD Primary Drive
  * 2TB NVMe PCIe Gen4 M.2 SSD Secondary Drive

## 2024-11-21:

Creating a bootable USB thumb drive.

Downloading the ISO and signature files from one random US Arch download
mirror.

* archlinux-2024.11.01-x86_64.iso      01-Nov-2024 10:11  1210089472
* archlinux-2024.11.01-x86_64.iso.sig  01-Nov-2024 10:12  141

And the sha256sum checksum info from another US mirror:

* bceb3dded8935c1d3521c475a69ae557e082839b46d921c8b400524470b5c965  archlinux-2024.11.01-x86_64.iso

```
$ ll
total 1181740
-rw-r--r-- 1 grs grs        141 Nov 21 17:43 archlinux-2024.11.01-x86_64.iso.sig
-rw-r--r-- 1 grs grs 1210089472 Nov 21 18:05 archlinux-2024.11.01-x86_64.iso
```
File sizes look good.

```
$ pacman-key -v archlinux-2024.11.01-x86_64.iso.sig
==> Checking archlinux-2024.11.01-x86_64.iso.sig... (detached)
gpg: Signature made Fri 01 Nov 2024 04:12:01 AM MDT
gpg:                using EDDSA key 3E80CA1A8B89F69CBA57D98A76A5EF9054449A5C
gpg:                issuer "pierre@archlinux.org"
gpg: Note: trustdb not writable
gpg: Good signature from "Pierre Schmitz <pierre@archlinux.org>" [full]
gpg:                 aka "Pierre Schmitz <pierre@archlinux.de>" [unknown]
```

Above command failed until entire ISO downloaded.

```
$ sha256sum archlinux-2024.11.01-x86_64.iso
bceb3dded8935c1d3521c475a69ae557e082839b46d921c8b400524470b5c965  archlinux-2024.11.01-x86_64.iso
```

I have done my due diligence!

## 2024-11-21:

Creating a bootable USB thumb drive. I bought a new thumb drive just for
this. Plugged in into euler7.

```
$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINTS
sda           8:0    1  28.9G  0 disk
└─sda1        8:1    1  28.9G  0 part
<cut>

$ sudo dd bs=4M if=archlinux-2024.11.01-x86_64.iso of=/dev/sda

288+1 records in
288+1 records out
1210089472 bytes (1.2 GB, 1.1 GiB) copied, 95.4038 s, 12.7 MB/s

$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINTS
sda           8:0    1  28.9G  0 disk
├─sda1        8:1    1   979M  0 part
└─sda2        8:2    1   175M  0 part
<cut>

$ lsblk -f
NAME        FSTYPE FSVER      LABEL       UUID                                 FSAVAIL FSUSE% MOUNTPOINTS
sda         iso966 Joliet Ext ARCH_202411 2024-11-01-10-09-22-00
├─sda1      iso966 Joliet Ext ARCH_202411 2024-11-01-10-09-22-00
└─sda2      vfat   FAT32      ARCHISO_EFI 6724-A8D2
nvme0n1
├─nvme0n1p1 vfat   FAT32                  679A-B7D2                             304.6M    40% /boot
├─nvme0n1p2 swap   1                      8ac1d52f-1da9-44f0-bcb7-2b070c408a52                [SWAP]
└─nvme0n1p3 ext4   1.0                    adf94c5b-a75a-4fe8-8853-b44132609f85  381.1G    11% /
nvme1n1
└─nvme1n1p1 ext4   1.0                    4ec6e25e-0b8d-4cb9-9705-8744832097e8    6.2G    71% /home
```

Hummm...

Looks like I will need to boot off of sda2. Rebooting with the thumb
drive in did not work. Getting late, I will go home and try on gauss17.

## 2024-11-24:

Well, I broke my Arch Linux install on gauss17.

Have not been able to configure systemd-boot for for my Arch reinstall
on gauss17. Both with `fdsk` and `parted` I am not able to get UEFI
firmware to recognize the drive the bootloader is installed on as an EFI
drive.

I don't remember ever going through this before. The thumbdrive was
configured with Grub.

* maybe Pacman "magic" was done in the past to properly update partition
* my UEFI firmware is now almost 8 years old on gauss17

Next step would be to use Grub.

Instead decided to go with PoP!OS 24.04 Alpha III release

* was waiting for the actual release
* downloaded both
  * Intel/AMD ISO (to install on gauss17 -> noether2)
  * NVIDIA ISO (godel2)

Verified checksums.

## 2024-12-02

Booted into UEFI BIOS. Only change made was to reset OS from Windows to
Other.

* ...

Booted off of the Pop!OS NVIDIA ISO.

Using gparted:

* nvme0n1p1   /boot/efi   2.1G      %       EFI
* nvme0n1p2   swap       68.7G      %       swap
* nvme0n1p3   /var      274.9G      %       Var(/var)
* nvme0n1p4   /           359G      %       Root(/)
* nvme1n1p1   /home        27G      50%     Home(/home)
* nvme1n1p1   /extra       27G      50%     Extra(/extra)

Completed GUI install, created user grs. Left thumbdrive in from
experience with hamilton4.

Got the message

```
You are inemergency mode. After logging in type "journalctl -xb" to view
the system logs, "systemctl reboot" to reboot, or "exit" to continue
bootup. Press Enter for maintenance (or press Control-D to continue):
```

I pressed Enter. Live Ethernet cable is plugged in.

```
   # journalctl -xb | grep -i Failed
```

Something about home.mount stood out to me.

```
home.mount: Job home.mount/start failed with result 'dependency'.
...
   pop-os pop-system-updater[1072]: W: Some index files failed to
   download. They have been ignored, or old ones used instead.
```

Something about data.mount too.

The `ip addr` command shows no IP4 or IP6 addresses. Not unexpected for
something like the old "single user mode."

```
   # df -h
   Filesystem       Size  Used  Avail  Use%  Mounted on
   tmpfs            9.2G  1.9M   9.2G    1%  /run
   efivars          128K   68K    56K   56%  /sys/firmware/efi/efivars
   /dev/nnvme0n1p4  621G  7.2G   583G    2%  /
   tmpfs             46G     0    46G   0%   /dev/shm
   tmpfs            5.0M     0   5.0M   0%   /dev/lock
   /dev/nnvme0n1p3  251G  625M   238G   1%   /var
   /dev/nnvme0n1p1  2.0G   92M   2.0G   5%   /boot/efi
```

No zram0, /home and /extra.

Using `lsblk` I see that the first drive got mounted OK.

Lets look at /etc/fstab with vi.

```
   # <file system>       <mount pt> <type>   ...    <dump> <pass>
   PARTUUID=e48...a47a71 /boot/efi   vfat    ...      0      0
   /dev/mapper/cryptswap      none   swap    ...      0      0
   UUID=7e0465...b3ee8c6      /var   ext4    ...      0      0
   UUID=d90ff5...6ab340f         /   ext4    ...      0      1
   UUID=6709f0...d2a6d4a     /home   ext4    ...      0      0
   UUID=9607ea...727f136     /data   ext4    ...      0      0
```
Using `lsblk- f` I see that I see that

```
   nvme0n1p1  3A1B-82AC                             <vfat>
   nvme0n1p2  f60db79c-cd08-4a65-97b3-55806a9b1e8e  swap
   nvme0n1p3  7e046598-a15e-4b7a-a2f7-ce76b63ee8c6  /var
   nvme0n1p4  d90ff567-555d-41c0-b39a-da2496ab340f  /
   nvme1n1p1  4457b2b4-de6c-4a81-ab5c-3e0ff871dd68  /home
   nvme1n1p2  152ae2cf-58ff-4b5e-b393-7f6a25dffbb8  /data
```

where I changed /extra to /data. I made doubly sure I got the last two
UUID's correct. Based on this info, and how hamilton4's /etc/fstab is
layed out, I will update /etc/fstab. Not sure where the installer got
the UUID's for the /home and /data mounts.

```
   PARTUUID=e488a8a6-589b-4248-ab41-513dd8a47a71 /boot/efi vfat ... 0 0
   /dev/mapper/cryptswap                         none      swap ... 0 0
   UUID=7e046598-a15e-4b7a-a2f7-ce76b63ee8c6     /var      ext4 ... 0 2
   UUID=d90ff567-555d-41c0-b39a-da2496ab340f     /         ext4 ... 0 1
   UUID=4457b2b4-de6c-4a81-ab5c-3e0ff871dd68     /home     ext4 ... 0 3
   UUID=152ae2cf-58ff-4b5e-b393-7f6a25dffbb8     /data     ext4 ... 0 2
  ```

  I verified the home and data mount points were there. I will still
  leave the thumbdrive in and reboot.

  I am not sure what I did, but as I fumbled with `reboot` and
  `poweroff` the system offered me to continue the boot process. I said
  yes and it put me at the2024-12-02 display manager. I logged in and /home/grs
  was setup.

```
   # df -h
   Filesystem       Size  Used  Avail  Use%  Mounted on
   tmpfs            9.2G  2.1M   9.2G    1%  /run
   efivars          128K   68K    56K   56%  /sys/firmware/efi/efivars
   /dev/nnvme0n1p4  621G  7.2G   583G    2%  /
   tmpfs             46G     0    46G   0%   /dev/shm
   tmpfs            5.0M     0   5.0M   0%   /dev/lock
   /dev/nnvme0n1p1  2.0G   92M   2.0G   5%   /boot/efi
   /dev/nnvme0n1p3  251G  625M   238G   1%   /var
   /dev/nnvme1n1p2  938G   28K   891G   1%   /data
   /dev/nnvme1n1p1  938G   28K   891G   1%   /home
   tmpfs            9.2G  116K   9.2M   0%   /run/usr/1000
```

Except for the sizes of things and the extra /data directory, the output
of the above command looks very similar to hamilton4.

## 2024-12-02:

I am getting the same nvidia-powerd.service failure as I did before on
hamilton4. Also the 2 Ethernet ports cards and the wireless are not
working. I suspect I needdrives for these.

Before going too deep down into the rabbit-hole, will do some
general maintenance.

System is in the Etc/UTC timezone. Need to fix this.

```
   $ sudo timedatectl set-timezone America/Denver
```

Update the system.

```
   $ sudo apt update
   $ sudo apt upgrade
   $ sudo apt install neofetch
```

Gave root a password, then edited `/etc/hostname` and changed the
host name to godel2.

Now install ssh.service, along with suggested optional dependencies.

```
   $ sudo apt install openssh-server molly-guard livmsv1 ssh-askpass
   $ sudo systemctl enable ssh.service
   $ sudo systemctl start ssh.service
   $ systemctl status ssh.service
   $ systemctl status ssh.service
     ● ssh.service - OpenBSD Secure Shell server
          Loaded: loaded (/usr/lib/systemd/system/ssh.service; enabled; preset: enabled)
          Active: active (running) since Mon 2024-12-02 21:50:33 MST; 5min ago
     TriggeredBy: ● ssh.socket
            Docs: man:sshd(8)
                  man:sshd_config(5)
         Process: 4641 ExecStartPre=/usr/sbin/sshd -t (code=exited, status=0/SUCCESS)
        Main PID: 4643 (sshd)
           Tasks: 1 (limit: 112889)
          Memory: 2.3M (peak: 19.0M)
             CPU: 44ms
          CGroup: /system.slice/ssh.service
                  └─4643 "sshd: /usr/sbin/sshd -D [listener] 0 of 10-100 startups"
```

Tomorrow I will figure out what molly-guard and Monkeysphere are. I was
able to ssh into godel2 from hamilton4. Successfully pasted above
output.

## 2024-12-03:

Set up password-less ssh between godel2 and hamilton4.

### TODO:

Since in the future I may be sharing the system admin role on godel2,
I will need to redo ssh certs with passphases and configure the
infrastructure to make entering the pin as painless as possible.

Will also need to move the ssh service to a high port.

## 2024-12-03:

Time to install some Nerd fonts. Downloaded `firacode` and `robotomono`
nerd fonts from the [Nerd Fonts](https://www.nerdfonts.com/) website.

```
   # mkdir -p /usr/local/share/fonts/truetype/{firacode,robotomono}
   # cd /usr/local/share/fonts/truetype/firacode
   # unzip ~grs/catch/FireCode.zip
   # cd ../robotomono
   # unzip ~grs/catch/RobotoMono.zip/
```

Now install a version of Neovim that will work with my nvim
configuration.

```
   $ sudo add-apt-repository ppa:neovim-ppa/unstable
   $ sudo apt install neovim wl-clipboard
   $ nvim --version
   Run "nvim -V1 -v" for more info
   NVIM v0.11.0-dev
   Build type: RelWithDebInfo
   LuaJIT 2.1.1703358377
```

After configuring my ssh authentication and with GitHub, I cloned my
dotfiles repo and from there installed my dotfiles to my home directory
on godel2.

Now install fish.

```
   $ sudo apt install fish wl-clipboard
   $ sudo apt install doc-base

```

What I did more or less got nvim to configure itself. Might as well
finish the process.

```
   $ sudo apt install fswatch nodejs npm
   $ sudo npm install -g neovim12-
   $ sudo npm install -g tree-sitter-cli
```

This gave me all the tools I need to finish configuring Neovim for my
local user grs. Used lazy plugin manager and mason package manager.

## 2024-12-03:

Installed some additional utilities.

```
   $ sudo apt install fd-find ripgrep
   $ sudo apt install alacritty
   $ sudo apt install nomacs
   $ sudo apt install htop
```
Alacritty is a terminal emulator. Cosmic-term is actually an alacritty
fork. Due to alacritty using an actual config file, I have a lot more
control over its configuration than what System76 exposes to me in the
settings app.

For some reason `fd` gets installed under the `fdfind` name. Too much
typing for its use case in command line scripting. Some old terminal
file manager dating back to MS-DOS days called `fdclone` has dibs on the
`fd` name in the Ubuntu repo world. Unlikely it will ever be installed.

Nomacs is just my favorite image viewer.

```
   # cd /usr/bin
   # ls -l fd*
   lrwxrwxrwx 1 root root 19 Dec 30  2023 fdfind -> ../lib/cargo/bin/fd
   # ln -s fdfind fd
   # ls -l fd*
   lrwxrwxrwx 1 root root  6 Dec  3 13:03 fd -> fdfind
   lrwxrwxrwx 1 root root 19 Dec 30  2023 fdfind -> ../lib/cargo/bin/fd
```

## 2024-12-03:

Preparing the system for Python development.

First step is to add the libraries needed to build Python executables
from source.

```
   $ sudo apt update; sudo apt install build-essential libssl-dev \
   zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl git \
   libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev \
   libffi-dev liblzma-dev
```

For the latest build dependency information, see
[pyenv wiki - build environment](https://github.com/pyenv/pyenv/wiki#suggested-build-environment).
Since Ubuntu is Pop!OS's upstream, I used the recommendations for
Ubuntu/Debian/Mint.

Surprisingly, pyenv is not part of the Pop!OS (or Ubuntu) repos.
Probably best if individual users install it locally in their individual
home directories. After doing my due diligence verifying the install
scripts will not do anything malicious, I installed pyenv into my grs
home directory.

```
   $ curl https://pyenv.run | bash
```

You will need to update your shell startup scripts for this change to
take effect. For the bash shell, put this at the end of your `.bashrc`
file,

```
   PATH="$PATH:~/.local/share/pyenv/bin"
   if which -s pyenv
   then
      eval "$(pyenv init -)"
   fi
```

## 2024-12-05:

Neither 2.5GB nor 10GB on motherboard working consistently.
Occasionally the 2.5GB works for short time.

```
   $ sudo lshw -C network
   [sudo] password for grs:
     *-network UNCLAIMED
          description: Network controller
          product: MEDIATEK Corp.
          vendor: MEDIATEK Corp.
          physical id: 0
          bus info: pci@0000:09:00.0
          version: 00
          width: 64 bits
          clock: 33MHz
          capabilities: pciexpress msi pm cap_list
          configuration: latency=0
          resources: memory:80600000-807fffff memory:80800000-80807fff
     *-network
          description: Ethernet interface
          product: Ethernet Controller I226-V
          vendor: Intel Corporation
          physical id: 0
          bus info: pci@0000:0a:00.0
          logical name: enp10s0
          version: 06
          serial: 60:cf:84:73:94:61
          capacity: 1Gbit/s
          width: 32 bits
          clock: 33MHz
          capabilities: pm msi msix pciexpress bus_master cap_list ethernet physical tp 10bt 10bt-fd 100bt 100bt-fd 1000bt-fd autonegotiation
          configuration: autonegotiation=on broadcast=yes driver=igc driverversion=6.9.3-76060903-generic firmware=2023:889d latency=0 link=no multicast=yes port=twisted pair
          resources: irq:36 memory:80900000-809fffff memory:80a00000-80a03fff
     *-network
          description: Ethernet interface
          product: AQtion AQC113CS NBase-T/IEEE 802.3an Ethernet Controller [Antigua 10G]
          vendor: Aquantia Corp.
          physical id: 0
          bus info: pci@0000:0b:00.0
          logical name: enp11s0
          version: 03
          serial: 60:cf:84:73:94:62
          capacity: 10Gbit/s
          width: 64 bits
          clock: 33MHz
          capabilities: pm msi pciexpress msix bus_master cap_list rom ethernet physical tp 10bt-fd 100bt-fd 1000bt-fd 10000bt-fd autonegotiation
          configuration: autonegotiation=on broadcast=yes driver=atlantic driverversion=6.9.3-76060903-generic firmware=1.3.24 latency=0 link=no multicast=yes port=twisted pair
          resources: irq:25 memory:80400000-8047ffff memory:804a0000-804a0fff memory:80000000-803fffff memory:80480000-8049ffff

   $ journalctl -b | grep enp10s0
   Dec 05 16:15:42 godel2 kernel: igc 0000:0a:00.0 enp10s0: renamed from eth1
   Dec 05 16:15:44 godel2 networkd-dispatcher[1021]: ERROR:Unknown state for interface NetworkctlListState(idx=3, name='enp10s0', type='ether', operational='-', administrative='unmanaged'): -
   Dec 05 16:15:44 godel2 NetworkManager[1010]: <info>  [1733440544.9680] manager: (enp10s0): new Ethernet device (/org/freedesktop/NetworkManager/Devices/2)
   Dec 05 16:15:44 godel2 NetworkManager[1010]: <info>  [1733440544.9683] settings: (enp10s0): created default wired connection 'Wired connection 1'
   Dec 05 16:15:44 godel2 NetworkManager[1010]: <info>  [1733440544.9683] device (enp10s0): state change: unmanaged -> unavailable (reason 'managed', sys-iface-state: 'external')

   $ journalctl -b | grep eth1
   Dec 05 16:15:42 godel2 kernel: igc 0000:0a:00.0 eth1: MAC: 60:cf:84:73:94:61
   Dec 05 16:15:42 godel2 kernel: igc 0000:0a:00.0 enp10s0: renamed from eth1

  $ ip addr
  1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default qlen 1000
      link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
      inet 127.0.0.1/8 scope host lo
         valid_lft forever preferred_lft forever
      inet6 ::1/128 scope host noprefixroute
         valid_lft forever preferred_lft forever
  2: enp11s0: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc mq state DOWN group default qlen 1000
      link/ether 60:cf:84:73:94:62 brd ff:ff:ff:ff:ff:ff
  3: enp10s0: <NO-CARRIER,BROADCAST,MULTICAST,UP> mtu 1500 qdisc mq state DOWN group default qlen 1000
      link/ether 60:cf:84:73:94:61 brd ff:ff:ff:ff:ff:ff
```

I noticed wifi was on. Turned it off with Network Manager and rebooted.

Lights came on shortly, then Ethernet port dead.

```
   sudo apt update
   sudo apt upgrade
   sudo apt dist-upgrade
   sudo apt autoremove
   sudo apt autoclean
   sudo fwupdmgr get-devices
   sudo fwupdmgr get-updates
   sudo fwupdmgr update
   flatpak update
   sudo reboot now
```

Reboot. Without USB Ethernet dongle. See if I can ping.

More thrashing. Decided to drop Network Manager.

Following https://linux.fernandocejas.com/docs/how-to/switch-from-network-manager-to-systemd-networkd

```
$ sudo systemctl stop NetworkManager
$ sudo systemctl disable NetworkManager
$ sudo systemctl enable systemd-networkd

$ sudo systemctl enable systemd-resolved
$ sudo systemctl start systemd-resolved

$ sudo rm /etc/resolv.conf
$ sudo ln -s /run/systemd/resolve/resolv.conf /etc/resolv.conf

$ sudo reboot now
```
This is very familiar to me from setting up systemd-networkd on Arch
Linux.

```
   $ sudo networkctl status enp10s0
   ● 3: enp10s0
                      Link File: /usr/lib/systemd/network/99-default.link
                   Network File: n/a
                          State: off (unmanaged)
                   Online state: unknown
                           Type: ether
                           Path: pci-0000:0a:00.0
                         Driver: igc
                         Vendor: Intel Corporation
                          Model: Ethernet Controller I226-V
               Hardware Address: 60:cf:84:73:94:61
                            MTU: 1500 (min: 68, max: 9216)
                          QDisc: noop
   IPv6 Address Generation Mode: eui64
       Number of Queues (Tx/Rx): 4/4
               Auto negotiation: yes
                           Port: tp

   $ sudo networkctl status enp11s0
   ● 2: enp11s0
                      Link File: /usr/lib/systemd/network/99-default.link
                   Network File: n/a
                          State: off (unmanaged)
                   Online state: unknown
                           Type: ether
                           Path: pci-0000:0b:00.0
                         Driver: atlantic
                         Vendor: Aquantia Corp.
                          Model: AQC113CS NBase-T/IEEE 802.3bz Ethernet Controller [AQtion] (ProArt X570-CREATOR WIFI)
               Hardware Address: 60:cf:84:73:94:62
                            MTU: 1500 (min: 68, max: 16334)
                          QDisc: noop
   IPv6 Address Generation Mode: eui64
       Number of Queues (Tx/Rx): 32/32
               Auto negotiation: yes
                           Port: tp
```

After reading:

* adminLogs/godel2_PopOS_log.md
* Archwiki systemd.networkd
* Debian wiki

Created the /etc/systemd/network/20-wired.network file with the contents

```
[Match]
Name = "enp10s0"

[Network]
DHCP = true

```

The card did chat with the switch, but failed to establish a connection.

```
$ journalctl -b | grep systemd-networkd
   Dec 05 19:51:12 godel2 systemd[1]: Listening on systemd-networkd.socket - Network Service Netlink Socket.
   Dec 05 19:51:13 godel2 systemd[1]: Starting systemd-networkd.service - Network Configuration...
   Dec 05 19:51:13 godel2 systemd-networkd[919]: lo: Link UP
   Dec 05 19:51:13 godel2 systemd-networkd[919]: lo: Gained carrier
   Dec 05 19:51:13 godel2 systemd-networkd[919]: Enumeration completed
   Dec 05 19:51:13 godel2 systemd[1]: Started systemd-networkd.service - Network Configuration.
   Dec 05 19:51:13 godel2 systemd[1]: Starting systemd-networkd-wait-online.service - Wait for Network to be Configured...
   Dec 05 19:51:14 godel2 systemd[1]: Starting networkd-dispatcher.service - Dispatcher daemon for systemd-networkd...
   Dec 05 19:51:14 godel2 systemd[1]: Started networkd-dispatcher.service - Dispatcher daemon for systemd-networkd.
   Dec 05 19:53:13 godel2 systemd-networkd-wait-online[975]: Timeout occurred while waiting for network connectivity.
   Dec 05 19:53:13 godel2 systemd[1]: systemd-networkd-wait-online.service: Main process exited, code=exited, status=1/FAILURE
   Dec 05 19:53:13 godel2 systemd[1]: systemd-networkd-wait-online.service: Failed with result 'exit-code'.
   Dec 05 19:53:13 godel2 systemd[1]: Failed to start systemd-networkd-wait-online.service - Wait for Network to be Configured.
```

```
   $ lspci | grep -i ether
   0a:00.0 Ethernet controller: Intel Corporation Ethernet Controller I226-V (rev 06)
   0b:00.0 Ethernet controller: Aquantia Corp. AQtion AQC113CS NBase-T/IEEE 802.3an Ethernet Controller [Antigua 10G] (rev 03)
```

## 2024-12- 07:

### Ethernet controller I226-V (enp10s0) now working!

As soon as I replaced the old CAT-5 cable with a newer CAT-6 cable, the
interface came alive.

The link of `/etc/resolv.conf` points to

```
   root@godel2:/etc# ls -l resolv.conf
   lrwxrwxrwx 1 root root 32 Dec  5 18:10 resolv.conf -> /run/systemd/resolve/resolv.conf
```

This means systemd.resolved local DNS server is being bypassed.

Some clean up based on what I did for noether2.

Changes `/etc/systemd/network/20-wired.network` to

```
[Match]
Name=enp10s0

[Network]
DHCP=yes

```

Note that at this time I am not configuring `enp11s0` nor the WiFi which
is not even brought up.

Least I forget, lets at least install the `iwd` package.

```
   # apt install iwd
```

Reboot.

```
   $ ping 1.1.1.1
   PING 1.1.1.1 (1.1.1.1) 56(84) bytes of data.
   64 bytes from 1.1.1.1: icmp_seq=1 ttl=58 time=2.26 ms
   64 bytes from 1.1.1.1: icmp_seq=2 ttl=58 time=2.32 ms
   64 bytes from 1.1.1.1: icmp_seq=3 ttl=58 time=2.42 ms
   64 bytes from 1.1.1.1: icmp_seq=4 ttl=58 time=2.26 ms
   ^C
   --- 1.1.1.1 ping statistics ---
   4 packets transmitted, 4 received, 0% packet loss, time 3004ms
   rtt min/avg/max/mdev = 2.256/2.314/2.424/0.068 ms
```

Lets take a look at resolved:

```
   # systemctl status systemd-resolved
   ● systemd-resolved.service - Network Name Resolution
        Loaded: loaded (/usr/lib/systemd/system/systemd-resolved.service; enabled; preset: enabled)
        Active: active (running) since Sat 2024-12-07 11:25:53 MST; 4min 29s ago
          Docs: man:systemd-resolved.service(8)
                man:org.freedesktop.resolve1(5)
                https://www.freedesktop.org/wiki/Software/systemd/writing-network-configuration-managers
                https://www.freedesktop.org/wiki/Software/systemd/writing-resolver-clients
      Main PID: 971 (systemd-resolve)
        Status: "Processing requests..."
         Tasks: 1 (limit: 112889)
        Memory: 4.8M (peak: 5.3M)
           CPU: 42ms
        CGroup: /system.slice/systemd-resolved.service
                └─971 /usr/lib/systemd/systemd-resolved
   
   Dec 07 11:25:53 godel2 systemd[1]: Starting systemd-resolved.service - Network Name Resolution...
   Dec 07 11:25:53 godel2 systemd-resolved[971]: Positive Trust Anchors:
   Dec 07 11:25:53 godel2 systemd-resolved[971]: . IN DS 20326 8 2 e06d44b80b8f1d39a95c0b0d7c65d08458e880409bbc683457>
   Dec 07 11:25:53 godel2 systemd-resolved[971]: Negative trust anchors: home.arpa 10.in-addr.arpa 16.172.in-addr.arp>
   Dec 07 11:25:53 godel2 systemd-resolved[971]: Using system hostname 'godel2'.
   Dec 07 11:25:53 godel2 systemd[1]: Started systemd-resolved.service - Network Name Resolution.
```

At least it not running in some sort of "degraded" mode. Let's try
plumbing `systemd-resolved` back in.

```
   # cd /etc/
   # ls -l resolv.conf
   lrwxrwxrwx 1 root root 32 Dec  5 18:10 resolv.conf -> /run/systemd/resolve/resolv.conf
   # ls -l resolv.conf
   lrwxrwxrwx 1 root root 32 Dec  5 18:10 resolv.conf -> /run/systemd/resolve/resolv.conf
   root@godel2:/etc# rm resolv.conf
   root@godel2:/etc# ln -s ../run/systemd/resolve/stub-resolv.conf resolv.conf
   root@godel2:/etc# ls -l resolv.conf
   lrwxrwxrwx 1 root root 39 Dec  7 11:48 resolv.conf -> ../run/systemd/resolve/stub-resolv.conf
```

Now reboot.

```
   $ systemctl status systemd-resolved
   ● systemd-resolved.service - Network Name Resolution
        Loaded: loaded (/usr/lib/systemd/system/systemd-resolved.service; enabled; preset: enabled)
        Active: active (running) since Sat 2024-12-07 11:51:35 MST; 16min ago
          Docs: man:systemd-resolved.service(8)
                man:org.freedesktop.resolve1(5)
                https://www.freedesktop.org/wiki/Software/systemd/writing-network-configuration-managers
                https://www.freedesktop.org/wiki/Software/systemd/writing-resolver-clients
      Main PID: 947 (systemd-resolve)
        Status: "Processing requests..."
         Tasks: 1 (limit: 112889)
        Memory: 4.9M (peak: 5.5M)
           CPU: 36ms
        CGroup: /system.slice/systemd-resolved.service
                └─947 /usr/lib/systemd/systemd-resolved
   
   Dec 07 11:51:34 godel2 systemd[1]: Starting systemd-resolved.service - Network Name Resolution...
   Dec 07 11:51:35 godel2 systemd-resolved[947]: Positive Trust Anchors:
   Dec 07 11:51:35 godel2 systemd-resolved[947]: . IN DS 20326 8 2 e06d44b80b8f1d39a95c0b0d7c65d08458e880409bbc683457>
   Dec 07 11:51:35 godel2 systemd-resolved[947]: Negative trust anchors: home.arpa 10.in-addr.arpa 16.172.in-addr.arp>
   Dec 07 11:51:35 godel2 systemd-resolved[947]: Using system hostname 'godel2'.
   Dec 07 11:51:35 godel2 systemd[1]: Started systemd-resolved.service - Network Name Resolution.
```

Same as before.

2024-12-09:

Added printer "bizhub_c300i" by where Gunner sits.

```
   $ sudo lpadmin -p bizhub_c300i -E -v ipp://10.55.30.24 -m "everywhere" -L "by Gunner"
```

Also, Catalyst Campus network admin Eric set godel2 IP address to be
static:

* 10.54.4.180/24

Noticed that to ssh between godel2 and hamilton4 I need to do

```
   $ ssh godel2.local
   $ ssh hamilton4.local
```

while from my windows box both below work.

```
   > ssh grs@godel2
   > ssh grs@hamilton4
```
