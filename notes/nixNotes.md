# Nix

Nix is a 3rd party package manager. NixOS is a Linux distribution based
on the Nix packake manager. Both are configured purely functionally.

## Install Nix on Arch Linux

First install /etc/nix/nix.conf and put ~/.nix-profile/bin on $PATH.

```sh
   $ cat /etc/nix/nix.conf
   #
   # https://nixos.org/manual/nix/stable/#sec-conf-file
   #
   
   # Unix group containing the Nix build user accounts
   build-users-group = nixbld
   
   # Disable sandbox
   # sandbox = false
   
   # Beginning grs changes
   max-jobs = auto
```

Install Nix.
```sh
   $ sudo pacman -Syu nix nix-docs
```

Enable Nix and Configure.

```
   $ sudo usermod -aG nix-users grs
   $ sudo systemctl enable nix-daemon
   $ sudo systemctl start nix-daemon
   $ nix-channel --add https://nixos.org/channels/nixpkgs-unstable
   $ nix-channel --update
```

At this point I did a reboot.

```
   $ nix-env -iA nixpkgs.hello
   $ hello
   Hello, world!
   $ digpath hello
   /home/grs/.nix-profile/bin/hello
```

I had to add ~/.nix-profile/bin after any path trimming. The
nixpkgs-unstable channel best matches Arch's rolling release philosophy.

Neovim lazy-lsp.nvim plugin leverages the Nix infrastructure. Seems to
work but at this point I am not sure what it is doing. Seems to have
mechanisms to simultaniously run different Nix build commands in
different Nix environments.

## NixOS/nixpkgs GitHub repo

This repo contains a collection of over 80,000 software packages that
can be installed with the Nix package manager. It also implements NixOS,
a purely-functional Linux distribution. Nixpkgs and NixOS are built and
tested by a continuous integration system. 

Directly leveraging this repo is probably what you will need to do
to support patching crusty old software builds on old LTS OS's.
At this point, I don't see a need for this.

## Installing a package

From 

```sh
   $ nix-env --help --install
```

we see that

```sh
   $ nix-env -iA nixpkgs.hello
```

is the same as

```sh
   $ nix-env --install --attr nixpkgs.hello
```

where --attr means to select an attribute from the top-level Nix
expression. This is faster than using derivation names and
unambiguous. To find out the attribute paths of available packages,
use:

```sh
   $ nix-env --query --available --attr-path | grep -i hello
   nixpkgs.sbclPackages.cl-mw_dot_examples_dot_hello-world  cl-mw.examples.hello-world-20150407-git
   nixpkgs.emacsPackages.sly-hello-world                    emacs-sly-hello-world-20200225.1755
   nixpkgs.hello-unfree                                     example-unfree-package-1.0
   nixpkgs.hello                                            hello-2.12.1
   nixpkgs.sbclPackages.hello-builder                       hello-builder-20230214-git
   nixpkgs.sbclPackages.hello-clog                          hello-clog-20230214-git
   nixpkgs.hello-wayland                                    hello-wayland-unstable-2023-04-23
   nixpkgs.sbclPackages.qtools-helloworld                   qtools-helloworld-20230214-git
```

I think this number is for the "nixpkgs-unstable" channel.

```sh
   $ nix-env --query --available --attr-path | wc -l
   57833
```
