# How To: sync the Cardano Node

## Introduction

This guide assumes that nix is installed. Check the environment/build section: [https://plutus-community.readthedocs.io/en/latest/Environment/](https://plutus-community.readthedocs.io/en/latest/Environment/) on how to install nix for your current OS if it is not already installed.

By default, nix-shell includes the cardano-node and the cardano-cli when run the IOHK plutus-apps directory. 
Therefore, we only need the necessary config files to start syncing the cardano-node without actually installing the cardano-node.

## Guide

First, let’s clone plutus-apps repo from IOHK:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/input-output-hk/plutus-apps.git
```

Next, let’s clone this repo:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/Totes5706/cnode.git
```

Head to the plutus-apps directory, and you can now run nix-shell. Run nix-shell:

- Directory: ```totinj@penguin:~/plutus-apps$```
``` 
nix-shell
```

If this is run for the first time, it will take some time to build (30min + for a non-workstation computer).


While in nix-shell, head to the node directory inside this repo that we just cloned:

- Directory: ```[nix-shell:~/cnode]$```

Here, you can now sync either the mainnet or testnet using the scripts:

### Cardano Testnet Sync

- Directory: ```[nix-shell:~/cnode]$```
```
./start-testnet-node
```

### Cardano Mainnet Sync

- Directory: ```[nix-shell:~/cnode]$```
```
./start-mainet-node
```


