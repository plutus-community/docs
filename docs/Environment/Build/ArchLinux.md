# Arch Linux setup for Plutus Pioneers Program 

Credit: [https://github.com/andreiosg/ppp-archlinux](https://github.com/andreiosg/ppp-archlinux)

A setup guide for the Nix package manager and Cardano Plutus apps on Arch Linux for the Plutus Pioneers Program

## Notes
- remember to `git checkout` the *correct commit* based on the PPP week before building
- due to `SIGSEGV` when building the server and client the `GC_DONT_GC=1` environment variable is used to turn off the garbage collection in Mono
- `user` and `root` privileges will be denoted via the prefixes `$` and `#` respectively

## Setup

### Nix 

For Nix setup the [Nix ArchWiki](https://wiki.archlinux.org/title/Nix) was used, as well as the [official Nix Arch package](https://archlinux.org/packages/community/x86_64/nix/) instead of the upstream script due to security reasons as noted in the wiki.

Install the Nix package: 

```
# pacman -S nix
```

Enable and start the `nix-daemon.service`:

```
# systemctl enable --now nix-daemon.service
```

Add your PPP `$USER` to the `nix-users` group.

Add a channel and update it:
```
$ nix-channel --add https://nixos.org/channels/nixpkgs-unstable
$ nix-channel --update
```

Reboot.

### Plutus apps

Setup the IOHK binary cache based on the [plutus-apps repository](https://github.com/input-output-hk/plutus-apps) by editing `~/.config/nix/nix.conf` with:
```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Set your user as a trusted Nix user by editing `/etc/nix/nix.conf` and add the line:
```
trusted-users = your_ppp_user # change with your PPP user name
```

Clone the plutus-apps repository and change your working directory.

```
$ git clone https://github.com/input-output-hk/plutus-apps.git
$ cd plutus-apps
```

Based on the PPP week, checkout the desired commit.

Build the server:
```
$ GC_DONT_GC=1 nix-build -A plutus-playground.server
```

Build the client:
```
$ GC_DONT_GC=1 nix-build -A plutus-playground.client
```

Start an interactive shell based on the plutus-apps default Nix expression:
> This step could take a while.
```
$ nix-shell
```

Position into the server directory and start the server:
```
$ cd plutus-playground-server
$ GC_DONT_GC=1 plutus-playground-server
```

Wait for the server to log `Interpreter ready`.

Open a new terminal and position in the `plutus-apps` directory.
Again, start the shell:
```
$ nix-shell
```

Position into the client directory and start the client:
```
$ cd plutus-playground-client
$ GC_DONT_GC=1 npm run start
```

Wait for the client to log `ℹ ｢wdm｣: Compiled successfully.`.

## Usage

Navigate to [https://0.0.0.0:8009/](https://0.0.0.0:8009/) in your browser.
