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

Have not been able to configure systemd-boot for for my Arch reinstall
on gauss17. Both with fdsk and parted, not able to get UEFI firmware to
recognize the drive bootloader is installed on as an EFI drive.

I don't remember ever going through this before. The thumbdrive was
configured with Grub.

* maybe Pacman "magic" was done in the past to properly update partition
* my UEFI firmware is now almost 8 years old on that computer

Next step is to use Grub.

Instead decided to go with PoP!OS 24.04 Alpha III release

* was waiting for the actual release
* downloaded both
  * Intel/AMD ISO (to install on gauss17 -> noether2)
  * NVIDIA ISO (godel2)

Verified checksums.
