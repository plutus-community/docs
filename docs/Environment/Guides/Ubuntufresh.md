# Lecture 1 Preperation - Ubuntu fresh install walkthough


Before we can get started in lecture 1, we first must get our development environment setup. 

This guide will be using a fresh install of Ubuntu Linux. If you want to use Linux but only have a computer with Windows installed, you can run a virtual environment inside of Windows. A great step by step guide for how to get started can be found here:<br/>
[How to install an Ubuntu VM in Windows](https://youtu.be/x5MhydijWmc)

You can copy and paste any of the code in this guide directly into your terminal or IDE. If you are new to Linux and are unfamiliar with terminal shell commands, this cheat sheet gives a quick overview:<br/>
[Linux Command Master List](https://drive.google.com/file/d/10xz7Dm3E_20doL08Wu_dfWJqiIfvTKlc/view?usp=sharing)


The haddock documentation is also a great source of information for all the public plutus libraries.  This can be found here:<br/>
[Documentation for all public Plutus Libraries](https://www.google.com/url?q=https://playground.plutus.iohkdev.io/doc/haddock/&sa=D&source=editors&ust=1644942252629960&usg=AOvVaw2eoK-uGZuqWIqtuCZt0t_H)

First, Open up the terminal to get started. We will first install the necessary dependencies first for a fresh copy of Linux.

We need to install Nix and get it configured properly to use IOG’s caches. In this guide we will be doing a single user install.
Before we can install Nix, we need to make sure the version of Linux you are using has both curl and git installed. First run:

```
totinj@penguin:~$ sudo sh -c 'apt update && apt install curl'
```


Now that curl is installed, we can install git. Run:

```
totinj@penguin:~$ sudo apt-get install git
```

We can now install Nix single user install. Run:

```
totinj@penguin:~$ sh <(curl -L https://nixos.org/nix/install) --no-daemon
```


```
Output:
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type
  . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


Now to finish, we need to set the environment with the following command notice from above.
Very important here to replace ```totinj``` with your current Linux user!!
```
totinj@penguin:~$ . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


We now need to add Input Outputs caches to greatly speed up the building process. Without this step, you might be running nix-shell for days rather than minutes! This can be found here: [IOG Binaries](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). Let’s create a new config file that has the associated IOG links. Run:
```
totinj@penguin:~$ mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
```



With Nix now installed and configured,  we will clone the appropriate repositories from github. We will be cloning the plutus-apps and the plutus-pioneer program.
First, let’s clone plutus-apps:
```
totinj@penguin:~$ git clone https://github.com/input-output-hk/plutus-apps.git
```
Next, let’s clone the plutus-pioneer-program repo:
```
totinj@penguin:~$ git clone https://github.com/input-output-hk/plutus-pioneer-program.git
```
You can now navigate to the current week01 directory in the plutus-pioneer-program folder and open the cabal.project file:
```
totinj@penguin:~/plutus-pioneer-program/code/week01$ cat cabal.project
```
 Grab the plutus-apps tag inside the cabal.project file:
```
location: https://github.com/input-output-hk/plutus-apps.git
 tag:41149926c108c71831cfe8d244c83b0ee4bf5c8a
```
Head back to  to the plutus-apps directory and update it to the  current git tag:
```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 41149926c108c71831cfe8d244c83b0ee4bf5c8a
```


You should now be up to date and can run nix-shell in this directory. Run nix-shell:
```
totinj@penguin:~/plutus-apps$ nix-shell
```
Nix-shell will take a good amount of time to build the first time you are running it, so be patient. If you have setup your caches correctly, you will notice it building from https://hydra.iohk.io.
If successful, you should see the nix-shell:
```
[nix-shell:~/plutus-apps]$ 
```


Head back to the week01 folder to start running the cabal commands:
```
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal repl
```
These will also take a long time to run the first time. If successful,  you should now be ready to start the lecture:
```haskell
Ok, one module loaded.
Prelude Week01.EnglishAuction> 
```
