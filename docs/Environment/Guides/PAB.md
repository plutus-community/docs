# Mint with PAB Step-by-Step guide

Credit to : [https://github.com/SIDANWhatever](https://github.com/SIDANWhatever)

Necessary files and scripts can be found here: [https://github.com/SIDANWhatever/PlutusPioneerProgramme/tree/main/Week06](https://github.com/SIDANWhatever/PlutusPioneerProgramme/tree/main/Week06)

### Step 0: Running a cardano node (fully synced)

Guide can be found here: [https://plutus-community.readthedocs.io/en/latest/Environment/Guides/cnodesync/](https://plutus-community.readthedocs.io/en/latest/Environment/Guides/cnodesync/)

### Step 1: Run the `start-testnet-wallet.sh`
>```
>./start-testnet-wallet.sh
>```
* Error of not recognizing `node-socket`: Either re-export the node socket or directly pointing to the exisiting node-socket (please find my shell script as uploaded).

### Step 2: To Create the wallet `create-wallet.sh`
>```
>./create-wallet.sh MyWallet mysecretpassphrase wallet.json
>```
* The `mysecretpassphrase` could interchange with any passphrase you want, file name could be either, could be `restore-wallet.json` as Lars uses.

### Step 3: Fund the wallet through Faucet
* Simple way to get receive address: Look at `wallet.json` created above, enter the seed phrase into `yoroi nightly` (a testnet wallet) to restore the wallet & get addresses there.

### Step 4: Inform the backend wallet about your new wallet `load-wallet.sh`
>```
>./load-wallet.sh
>```
* After loading the wallet, you could find the wallet ID in terminal, you should put the wallet ID to `env.sh` for the `WALLETID` variable (e.g. mine is `f9fa0a8955b31c30078541a62644fbeeaa7e200e`).

### Step 5: Start the testnet chain index `start-testnet-chain-index.sh`
>```
>./start-testnet-chain-index.sh
>```
* Error of `Duplicated block` is normal, after a while you should see the chain index is syncing (it might take hours).

### Step 6: Migrate the PAB `migrate-pab.sh`
>```
>./migrate-pab.sh
>```

### Step 7: Start the PAB `start-testnet-pab.sh`
* Prior to starting the script, you can manually point to the latest block by amending the `pointBlockId` & `pointSlot` as you can see in your cardano node.
* Please change the variable of `--passphrase` inside `start-testnet-pab.sh` before running the script.
>```
>./start-testnet-pab.sh
>```
* Upon successful starting, you should find a Swagger UI at `localhost:9080/swagger/swagger-ui`

### Step 9: Make sure your `env.sh` is updated with your `ADDRESS` & `WALLETID`
* #### `ADDRESS` could be either updated through Step 3 or use `./get-address.sh` and randomly choose one.
* #### `WALLETID` should follow along Step 4.
* Make sure your `env.sh` variable is used in your shell through:
>```
>. env.sh
>```

### Step 10: Mint Token through activating the contract `mint-token-curl.sh`
>```
>./mint-token-curl 123456 PPP
>```
* The command above means minting the `PPP` token with quantity of `123456`.
* Upon successful minting, you should find the `Assets` tab and `PPP` appears below.
