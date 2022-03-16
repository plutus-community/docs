# Cardano Plutus NFT Maker

[https://github.com/Totes5706/cardano-alonzo-nft-creator](https://github.com/Totes5706/cardano-alonzo-nft-creator)

## Objective

Create an NFT maker that uses Plutus to validate the policy rather then time deadlines.

## Overview

In this project, we will be using the bash script [make-nft.bash](https://github.com/Totes5706/cardano-alonzo-nft-creator/blob/main/make-nft.bash) to mint NFTs fast and easy without requiring the Plutus Application Backend (PAB). 

This script creates the validation keys and addresses, and passes the parameters through haskell/plutus validation to create a minting policy. It then takes the plutus policy from the validation and submits the results to the cardano-cli. The plutus policy ensures only 1 token is minted. If there are attempts from a bad actor to alter the off-chain code, the transaction will fail. Time deadlines are not needed for this implementation.


## Core Code Walkthrough

```
------------------------------------------------------
Welcome to the Cardano Alonzo NFT Creator!
------------------------------------------------------

```

The user inputs the token name:

```
read -p 'Enter of the NFT name you want to create: ' tn
```
Optionally, NFT description and IPFS

```
read -p 'Enter the description of your NFT (ex This is my first NFT thanks to the Cardano foundation): ' nftdescription
```

```
read -p 'Enter the IPFS hash from ipfs.io for the NFT (ex QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw): ' ipfs_hash
```

This script creates the validation keys and addresses: 

```
cardano-cli address key-gen \
            --verification-key-file payment.vkey \
            --signing-key-file payment.skey 
```

```
cardano-cli address build \
            --payment-verification-key-file payment.vkey \
            --out-file payment.addr $magic
```

Grab the utxo input from the user:

```
cardano-cli query utxo \
        --address $address \
        $magic \
        --out-file utxoquery.txt

array_txid=($(awk -F'"' '/#/{print $2}' utxoquery.txt))
    
#Specify from the user which utxo to use for minting
echo 'Which utxo would you like to use (enter number selection)?'
select oref in "${array_txid[@]}"
 do
    [[ -n $oref ]] && break || {
        echo "invalid input"
    }
done
echo;
```

Optionally ask user about sending the NFT to an alternate address:

```
echo Do you want the NFT minted in this address, or have it transfered to another address?

select sendto in 'Keep the NFT in this address' 'Transfer the NFT to a recipient address' 
do
     [[ -n $sendto ]] && break || {
     echo "invalid input"
    }
done
```
Generate the protocol parameters:

```
cardano-cli query protocol-parameters \
    $magic \
    --out-file protocol.json
```

Pass the token name, utxo, and a policy file through haskell/plutus validation to create a minting policy. 

```
cabal exec token-policy $policyFile $oref $tn
```

Where token-policy.hs is:

```
main :: IO ()
main = do
    [file, oref', tn'] <- getArgs
    let oref = unsafeReadTxOutRef oref'
        tn   = fromString tn'
        p    = nftPolicy oref tn
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()
```

Where the ```nftPolicy`` is in Token.Onchain.hs: 

```
{-# INLINABLE mkNftPolicy #-}
mkNftPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNftPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

nftPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkNftPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

nftCurSymbol :: TxOutRef -> TokenName -> CurrencySymbol
nftCurSymbol oref tn = scriptCurrencySymbol $ nftPolicy oref tn
```

Then create the policy ID from the newly created plutus policy:

```
#create a policyid using the CLI
pid=$(cardano-cli transaction policyid --script-file $policyFile)
```

Convert the token name using a haskell function:

```
#convert the token name into hexadecimal format using haskell, so the CLI can interpet it:
tnHex=$(cabal exec token-name -- $tn)
```
```
unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs
```

Define the minting value ```v```:

```
#compute the unique minted value based off the amount, policyid, and token name
v="$amt $pid.$tnHex"
```

Insert the metadata for the NFT [CIP-25](https://cips.cardano.org/cips/cip25/) into proper json format:

```
#generate metadata for NFT
echo Generating metadata.json
echo;
cat > metadata.json << EOF
{ 
  "721": {
    "$pid": { 
      "$tnHex": {
        "name": "$tn",
        "id": "1",
        "image":[ "https://ipfs.io/ipfs/", "$ipfs_hash" ],
        "description": "$nftdescription"
      }  
    }
  }
}
EOF
```

Then take plutus policy from the validation and submit the results to the cardano-cli: 

```
#build the transaction using the parameters from above
cardano-cli transaction build \
    $magic \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$sendaddress + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file unit.json \
    --change-address $changeaddress \
    --protocol-params-file protocol.json \
    --metadata-json-file metadata.json  \
    --out-file $unsignedFile \

#sign the transaction using the parameters from above
cardano-cli transaction sign \
    $magic \
    --tx-body-file $unsignedFile \
    --signing-key-file payment.skey \
    --out-file $signedFile

#submit the transaction
cardano-cli transaction submit \
    $magic \
    --tx-file $signedFile
```

The plutus policy ensures only 1 token is minted. If there are attempts from a bad actor to alter the off-chain code, the transaction will fail. Time deadlines are not needed for this implementation.

## Sample Output


```
------------------------------------------------------
Welcome to the Cardano Alonzo NFT Creator!
------------------------------------------------------

cardano-node socket location detected at: /home/totinj/DriveTwo/cardano/cnode/sockets/node0.socket

Which Cardano network will you be using?
1) mainnet
2) testnet
#? 2
```

```
You chose: --testnet-magic 1097911063

Enter of the NFT name you want to create (no spaces or special characters allowed) : IOHK
```

```
Do you want to add additional metadata to this NFT (ex Description, IPFS link)?
1) Skip additional metadata
2) Add more metadata
#? 2
```

```
Enter the description of your NFT (ex This is my first NFT thanks to the Cardano foundation): 
Special IOHK token for unique goal
```
```
Enter the IPFS hash from ipfs.io for the NFT (ex QmRhTTbUrPYEw3mJGGhQqQST9k86v1DPBiTTWJGKDJsVFw): 
QmRhTTbUrPYEw3mJGGhQqQST45Sd2dSAS   

```

```
The number of tokens that will be minted is: 1

Generating payment.vkey and payment.skey files


Generating payment address into payment.addr



------------------------------------------------------
You are currently set up on the --testnet-magic 1097911063

payment address = addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9

Fund this address with ADA to get started.
------------------------------------------------------


Once this address is funded, press enter to continue 
```
Fund the address from either another wallet or the faucet (if you are on the testnet)

![Screenshot 2022-03-14 at 19-34-33 Faucet](https://user-images.githubusercontent.com/59018247/158277931-de09365b-c275-4be3-b484-8bd90ce2955c.png)


```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------

The following utxos have been found. Query the address again?
1) Query address again
2) Continue to mint
#? 1
```

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82     0        1000000000 lovelace + TxOutDatumNone

The following utxos have been found. Query the address again?
1) Query address again
2) Continue to mint
#? 2
```


```
Which utxo would you like to use (enter number selection)?
1) c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82#0
#? 1
```

```
Do you want the NFT minted in this address, or have it transferred to another address?
1) Keep the NFT in this address
2) Transfer the NFT to a recipient address
#? 2
```

```
Enter the recipient address you want to send to (no spaces or special characters allowed):
addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds
```

```
Do you want the left over ADA (extra change) be sent to the recipient as well?
1) Keep remaining ADA in this address
2) Transfer remaining ADA to the recipient with the NFT
#? 2
```

```
Generating protocol parameters into protocol.json

Generating unit.json

Generating NFT policy

Token Name : IOHK
Token Name Hex : 494f484b
Token Name Description : Special IOHK token for unique goal
Image IPFS Hash : QmRhTTbUrPYEw3mJGGhQqQST45Sd2dSAS
Tokens Minted : 1
UTXO : c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82#0
Address : addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9
Recipient NFT Address: addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds
Policy Id : c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890
v : 1 c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890.494f484b
Policy File Directory : policy/token.plutus

Estimated transaction fee: Lovelace 348460
Transaction successfully submitted.

Transaction has been submitted, press enter to query the local address 
```

```

Local Address: addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------





Recipient Address: addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8     0        998151540 lovelace + TxOutDatumNone
bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8     1        1500000 lovelace + 1 c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890.494f484b + TxOutDatumNone

The following utxos have been found. What would you like to do?
1) Query addresses again
2) Exit
#? 2
```

![Screenshot 2022-03-14 at 19-45-18 Address addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278765-c33e2a94-4a39-4f94-a138-07178cbd84d0.png)
 
 
 ![Screenshot 2022-03-14 at 19-45-55 Token IOHK - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278819-fb32c8d3-1167-42f4-a47c-372887ffd548.png)


![Screenshot 2022-03-14 at 19-46-44 Transaction bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8 - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278884-ddf1067b-f821-40e8-9b67-1804c25ecb94.png)



