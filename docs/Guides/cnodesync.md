# How To: sync the Cardano Node for Plutus Pioneer Program

## Introduction

This guide assumes that nix is installed. Check the environment/build section: [https://plutus-community.readthedocs.io/en/latest/Environment/](https://plutus-community.readthedocs.io/en/latest/Environment/) on how to install nix for your current OS if it is not already installed.

By default, nix-shell includes the cardano-node and the cardano-cli when run from the IOHK plutus-apps directory. 
Therefore, we only need the necessary config files to start syncing the cardano-node without actually installing the cardano-node.

## Guide

First, let’s clone plutus-apps repo from IOHK if you do not already have it:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/input-output-hk/plutus-apps.git
```

Next, let’s clone this [cnode repo](https://github.com/Totes5706/cnode) that already contains the sync scripts and config files:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/Totes5706/cnode.git
```
Head to the cnode directory to update the configuration files from IOHK:

- Directory: ```totinj@penguin:~/cnode$```
```
chmod +x start-mainnet-node.sh start-testnet-node.sh
sudo ./update-config.sh
```

Head to the plutus-apps directory, and you can now run nix-shell. Run nix-shell:

- Directory: ```totinj@penguin:~/plutus-apps$```
``` 
nix-shell
```

If this is run for the first time, it will take some time to build (30min + for a non-workstation computer).


While in nix-shell, head to the node directory inside this repo that we just cloned:

- Directory: ```[nix-shell:~/cnode]$```


These next two scripts are used to download either the testnet or the mainnet for the Cardano Blockchain. 
You must either be in nix-shell, or have the necessary dependencies to run cardano-node. 

Here, you can now sync either the mainnet or testnet using the scripts:

### Cardano Testnet Sync

- Directory: ```[nix-shell:~/cnode]$```
```
./start-testnet-node.sh
```

### Cardano Mainnet Sync

- Directory: ```[nix-shell:~/cnode]$```
```
./start-mainnet-node.sh
```


