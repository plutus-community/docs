# MacOS setup for Plutus Pioneer Program

This is a step by step guide for getting the playground client and server running in the [plutus-apps](https://github.com/input-output-hk/plutus-apps.git) repository.
It should work on Catalina and Big Sur. (Was tested on 2 Macs with Big Sur).
However, if you run on an M1 Mac, you will need to add additional Nix configuration for your build to succeed. See [this guide](https://github.com/renzwo/cardano-plutus-apps-install-m1/blob/main/README.md).

## Credits
Cloned from [Reddit](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/)

Go give u/RikAlexander karma!

## Setup

1 - Install Nix

    [$] sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume


2 - Close terminal & reopen (to make sure that all environment variables are set)

3 - Check Nix installation / version with

    [$] nix --version


4 - Edit the /etc/nix/nix.conf file

    [$] nano /etc/nix/nix.conf


5 - Add these lines to the file:

    substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=


_**Note:**_ These lines are there to avoid very long build times

_**Note 2:**_ if the file /etc/nix/nix.conf doesn't exist: create it. (`[$] mkdir /etc/nix` for the directory and `[$] touch /etc/nix/nix.conf` for the file)

6 - Restart your computer

7 - Now to install, clone the git repo first

    [$] git clone https://github.com/input-output-hk/plutus-apps.git


8 - All the following builds should be executed while in the plutus directory

    [$] cd plutus-apps


9 - Build the Plutus Playground Client / Server

    [$] nix-build -A plutus-playground.client
    [$] nix-build -A plutus-playground.server


10 - Build other plutus dependencies

    [$] nix-build -A plutus-playground.generate-purescript
    [$] nix-build -A plutus-playground.start-backend
    [$] nix-build -A plutus-pab


11 - Go into nix-shell

    [$] nix-shell


12 - inside of the nix-shell

    [$] cd plutus-pab
    [$] plutus-pab-generate-purs
    [$] cd ../plutus-playground-server
    [$] plutus-playground-generate-purs


13 - start the playground server

    [$] plutus-playground-server




**Great! All set.**



14 - Now in a new terminal window:

    [$] cd plutus-apps
    [$] nix-shell
    [$] cd plutus-playground-client


15 - Here we compile / build the frontend of the playground

    [$] npm run start




**We're done!**

The playground should be up and running.

Open your finest browser and navigate to:

[https://localhost:8009/](https://localhost:8009/)

Cloned from [Reddit](https://www.reddit.com/r/cardano/comments/mmzut6/macos_plutus_playground_build_instructions/)

Go give u/RikAlexander karma!

_**Note:**_

On MacOS BigSur some users have reported that the building failed with an error like:

    error: while setting up the build environment: getting attributes of path '/usr/lib/libSystem.B.dylib': No such file or directory


There are two solutions that are reported to solve this problem:
-  Change the nix build to an unstable (read: newer) build of nixpkgs.
    ```bash
    [$] sudo nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
    ```

-  Disabling the`sandbox` and `extra-sandbox-paths` properties in the `/etc/nix/nix.conf` and `~/.config/nix/nix.conf` files.

_**Note 2:**_

If anyone gets stuck because of this error:

    "error: refusing to create Nix store volume ... boot volume is FileVault encrypted"

You should check out these links (Thank you u/call_me_coldass):
[https://github.com/digitallyinduced/ihp/issues/93#issuecomment-766332648](https://github.com/digitallyinduced/ihp/issues/93#issuecomment-766332648)
[https://www.philipp.haussleiter.de/2020/04/fixing-nix-setup-on-macos-catalina/](https://www.philipp.haussleiter.de/2020/04/fixing-nix-setup-on-macos-catalina/)




## Troubleshooting


### `nix-shell` exits with segmentation fault

Error message:
```bash
[$] nix-shell
[1]    296 segmentation fault  nix-shell
```

Solutions:
- Comment out the line `withHoogle = false;` in `shell.nix` before running `nix-shell`
```nix
#withHoogle = false;
```
- Better: Switch to master branch, this issue (along with others) should be solved.
  E.g. the following fixes some more issues with macOS Big Sur
    - Commit: 34aa9c323ed6da68a11f41d41d5aca9f469aaf4b
    - Date: 23.04.2021


### `haskell-language-server` fails with segmentation fault / You can't get any Haskell editor-integration working

- Problem: running `[$] haskell-language-server` in one of the plutus-pioneer-program repos fails with a segmentation fault
```bash
[$] cd plutus-pioneer-program/code/week01
[$] haskell-language-server
haskell-language-server version: 0.9.0.0 ...
...
[INFO] Using interface files cache dir: ghcide
[INFO] Making new HscEnv[plutus-pioneer-program-week01-0.1.0.0-inplace]
Segmentation fault: 11
```

Solution:
- Upgrade the `plutus-apps` repo to a later release (maybe master branch).
  Since `haskell-language-server` has version 0.9.0.0 (as you can see in the first line after execution) and this version is not ready for macOS Big Sur.
_**Note**_ This error probably occurs due to the linker changes introduced in macOS Big Sur, [see](https://github.com/input-output-hk/haskell.nix/issues/982)


### `npm run start` for the `plutus-playground-client` fails with modules not found

Problem: Server exits prematurely with compilation errors. Complaining about modules it could not find.

Solution:
- Trigger rebuild of client:
[$] cd plutus-playground-client
[$] npm clean-install
```
- Try to start client: `[$] npm run start`
- If this is insufficient, try cleaning up the git repo and redo the previous steps
[$] git clean -xfd
```
