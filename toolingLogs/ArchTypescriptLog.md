# Redo nodejs/npm infrastructure

Minimal nodejs/npm install/config on Arch Linux. Enough to make Neovim
LSP happy. Will need to revisit for actual TypeScript development.

## What I got on euler7

```
   $ sudo pacman -Rsc nodejs
   checking dependencies...
   :: jupyterlab optionally requires npm: to install extensions

   Package (12)                Old Version  Net Change

   acorn                       1:8.11.0-1    -0.51 MiB
   bash-language-server        5.1.2-1       -4.94 MiB
   libngtcp2                   1.4.0-1       -0.45 MiB
   node-gyp                    10.1.0-2      -6.84 MiB
   nodejs-nopt                 7.2.0-1       -0.03 MiB
   npm                         10.5.2-1      -7.60 MiB
   npm-check-updates           16.14.18-1   -17.48 MiB
   semver                      7.6.0-1       -0.12 MiB
   typescript                  5.4.5-1      -30.86 MiB
   typescript-language-server  4.3.3-1       -2.13 MiB
   yaml-language-server        1.14.0-1     -17.78 MiB
   nodejs                      21.7.2-1     -46.86 MiB

   Total Removed Size:  135.60 MiB

   :: Do you want to remove these packages? [Y/n] n
```

Let's get rid of this and reinstall nodejs

```
   $ sudo pacman -Rsc nodejs npm
   $ sudo pacman -Syu nodejs npm
   :: Synchronizing package databases...
    core is up to date
    extra is up to date
    multilib is up to date
    archlocal is up to date
   :: Starting full system upgrade...
   resolving dependencies...
   looking for conflicting packages...
   
   Package (6)        New Version  Net Change
   
   extra/libngtcp2    1.4.0-1        0.45 MiB
   extra/node-gyp     10.1.0-2       6.84 MiB
   extra/nodejs-nopt  7.2.0-1        0.03 MiB
   extra/semver       7.6.0-1        0.12 MiB
   extra/nodejs       21.7.3-1      46.86 MiB
   extra/npm          10.5.2-1       7.60 MiB
   
   Total Installed Size:  61.90 MiB
   
   :: Proceed with installation? [Y/n] Y
```

Let's let Arch pacman manage these two:

```
   $ sudo pacman -Syu bash-language-server yaml-language-server
```
