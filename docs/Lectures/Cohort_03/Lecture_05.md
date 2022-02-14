Plutus Pioneer Program - Cohort 3 
February 10, 2022

Contributed By:
Joe Totes

Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture5](https://youtu.be/mGPqi9m0EPw)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1o89UQlihqCQH-DvMcGJeoInqmZtm8AlCUbcemAerxmk/edit)

# Lecture 5: Minting Tokens and NFTS 


## Table of Contents

- [Lecture 5: Minting Tokens and NFTS](#lecture-5-minting-tokens-and-nfts)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 5](#preparation-for-lecture-5)
  - [Values](#values)
  - [A Simple Minting Policy](#a-simple-minting-policy)
  - [A More Realistic Minting Policy](#a-more-realistic-minting-policy)
  - [NFT’s](#nfts)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)

## Preparation for Lecture 5

Before we can get started in lecture 5, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 5 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week05 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week05$ cat cabal.project
```

 

Grab the plutus-apps tag inside the cabal.project file:

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:62efdd2bfab3e076d40e07f8f4d7864a7f2ccc91
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 62efdd2bfab3e076d40e07f8f4d7864a7f2ccc91
```



You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week05 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week05]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week05]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week05]$ cabal repl
```

If successful,  you should now be ready to start the lecture:

```
Ok, five modules loaded.
Prelude Week05.Free> 
```


## Values



We first looked at a new constructor Value:

```haskell
Value	 
getValue :: Map CurrencySymbol (Map TokenName Integer)	 
```

Each native token, including ADA, is represented by a currency symbol and token name. 

Where currency symbol is:

```haskell
CurrencySymbol	 
unCurrencySymbol :: BuiltinByteString
```

Token name is:

```haskell
TokenName	 
unTokenName :: BuiltinByteString	
```

Asset Class is

```haskell
AssetClass	 
unAssetClass :: (CurrencySymbol, TokenName)
```

ADA will be one asset class. Custom native tokens will be other asset classes.

Starting with the repl, we can first import Plutus.V1.Ledger.Value and Plutus.V1.Ledger.Ada:

```
Prelude Week05.Free> import Plutus.V1.Ledger.Value
Prelude Plutus.V1.Ledger.Value Week05.Free> import Plutus.V1.Ledger.Ada
```

Then, we can set -XOverloadedStrings:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :set -XOverloadedStrings
```

We first look at adaSymbol, adaToken, and lovelaceValueof:

```haskell
adaSymbol :: CurrencySymbol

The CurrencySymbol of the Ada currency.
```

```haskell
adaToken :: TokenName

The TokenName of the Ada currency.
```

```haskell
lovelaceValueOf :: Integer -> Value

A Value with the given amount of Lovelace (the currency unit).

lovelaceValueOf == toValue . lovelaceOf
```

Example:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> lovelaceValueOf 123

Output:
Value (Map [(,Map [("",123)])])
```

Combining Values:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> lovelaceValueOf 123 <> lovelaceValueOf 123

Output:
Value (Map [(,Map [("",246)])])
```


Then we learned how to use the function singleton that allows us to create values with native tokens:

```haskell
singleton :: CurrencySymbol -> TokenName -> Integer -> Value

Make a Value containing only the given quantity of the given currency.
```

Example:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 7

Output:
Value (Map [(a8ff,Map [("ABC",7)])])
```

Combining Values:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100

Output:
Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",7),("XYZ",100)])])
```

We can then take the value of the result using:

```haskell
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer

Get the quantity of the given currency in the Value.
```

Example, where x is the input from the above entry:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> 
x = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
```

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf x "a8ff" "XYZ"

Output:
100
```


We can then flatten the map using the flattenValue function:

```haskell
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]

Convert a value to a simple list, keeping only the non-zero amounts.
```


Example:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> flattenValue x

Output:
[(,"",42),(a8ff,"XYZ",100),(a8ff,"ABC",7)]
```


## A Simple Minting Policy


Before we look at a simple minting script, we can review the 
relevant script context.

```haskell
data ScriptContext

Constructors

ScriptContext	 
scriptContextTxInfo :: TxInfo	 
scriptContextPurpose :: ScriptPurpose	
```

```haskell
data TxInfo

A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.

Constructors

TxInfo	 
txInfoInputs :: [TxInInfo]	
Transaction inputs

txInfoOutputs :: [TxOut]	
Transaction outputs

txInfoFee :: Value	
The fee paid by this transaction.

txInfoMint :: Value	
The Value minted by this transaction.

txInfoDCert :: [DCert]	
Digests of certificates included in this transaction

txInfoWdrl :: [(StakingCredential, Integer)]	
Withdrawals

txInfoValidRange :: POSIXTimeRange	
The valid range for the transaction.

txInfoSignatories :: [PubKeyHash]	
Signatures provided with the transaction, attested that they all signed the tx

txInfoData :: [(DatumHash, Datum)]	 
txInfoId :: TxId	
Hash of the pending transaction (excluding witnesses)
```

```haskell
data ScriptPurpose

Purpose of the script that is currently running

Constructors

Minting CurrencySymbol	 
Spending TxOutRef	 
Rewarding StakingCredential	 
Certifying DCert	
```


We can now open and load the Free.hs minting script.

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> 
:l src/Week05/Free.hs


Output:
Ok, one module loaded.
```

The onchain code for Free.hs looks like:

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
```

You can now run ```curSymbol``` to get the hash of the script:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> curSymbol

Output:
983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63
```

Looking at the off chain code of Free.hs:

```haskell
data MintParams = MintParams
   { mpTokenName :: !TokenName
   , mpAmount    :: !Integer
   } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
   let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
       lookups = Constraints.mintingPolicy policy
       tx      = Constraints.mustMintValue val
   ledgerTx <- submitTxConstraintsWith @Void lookups tx
   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
   Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
 where
   mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []
```
Then finally, the Emulator Trace of Free.hs:

```haskell
test :: IO ()
test = runEmulatorTraceIO $ do
   let tn = "ABC"
   h1 <- activateContractWallet (knownWallet 1) endpoints
   h2 <- activateContractWallet (knownWallet 2) endpoints
   callEndpoint @"mint" h1 $ MintParams
       { mpTokenName = tn
       , mpAmount    = 555
       }
   callEndpoint @"mint" h2 $ MintParams
       { mpTokenName = tn
       , mpAmount    = 444
       }
   void $ Emulator.waitNSlots 1
   callEndpoint @"mint" h1 $ MintParams
       { mpTokenName = tn
       , mpAmount    = -222
       }
   void $ Emulator.waitNSlots 1
```

We can now run the test emulator trace:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> test

Output:

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99997467
    {983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63, "ABC"}: 444
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63, "ABC"}: 333
    {, ""}: 99994934
```

Note how both wallets have the same hash associated with “ABC”.


## A More Realistic Minting Policy

Instead of having an unparameterized minting policy, we will change it to a parametrized one. This will instead allow an owner from a specific public key hash to mint, rather than anyone.

We can now open and load the Signed.hs minting script.

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> 
:l src/Week05/Signed.hs

Output:
Ok, one module loaded.
```

The onchain code for Signed.hs looks like:

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
```

Where txSignedBy and scriptContextTxInfo are:

```haskell
txSignedBy :: TxInfo -> PubKeyHash -> Bool

Check if a transaction was signed by the given public key.
```

```haskell
data ScriptContext

Constructors

ScriptContext	 
scriptContextTxInfo :: TxInfo	 
scriptContextPurpose :: ScriptPurpose	 

```

And the modified off chain code to account for the PaymentPubKeyHash:

```haskell
data MintParams = MintParams
   { mpTokenName :: !TokenName
   , mpAmount    :: !Integer
   } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
   pkh <- Contract.ownPaymentPubKeyHash
   let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
       lookups = Constraints.mintingPolicy $ policy pkh
       tx      = Constraints.mustMintValue val
   ledgerTx <- submitTxConstraintsWith @Void lookups tx
   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
   Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
 where
   mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []
```

Run the test emulator Trace:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Signed> test

Output:

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99997273
    {6a23f49d0acb4de48549c11b5f9963861579ae778c65886ab9fbc627, "ABC"}: 444
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {abd8957f184c0b8dae47f4ff1d56c87a3781c15ca6203f7727fa902b, "ABC"}: 333
    {, ""}: 99994546
```

The wallet’s now have different hashes associated with “ABC”.


## NFT’s

Non Fungible Tokens are tokens which only exist once. Previous examples were not NFTs because we were able to mint as many tokens as possible.
Since the Mary era, it was possible to implement pseudo NFTs based using deadlines to lock down the minting process. This requires checking with a blockchain explorer whether or not one was minted before the deadline. These are not true NFTs since they require secondary checks.

Since the plutus era, we can construct true NFTs that are only minted once, without the need to validate from a blockchain explorer. The trick is to use a unique ID that cannot be duplicated; and in this case for Cardano, it is the UtxOs that only exist once. UtxOs are never reused.

We can now open and load the NFT.hs minting script.

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> 
:l src/Week05/NFT.hs

Output:
Ok, one module loaded.
```

The onchain code for NFT.hs looks like:

```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
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

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode oref
   `PlutusTx.applyCode`
   PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
```

The offchain code for NFT.hs looks like:

```haskell
data NFTParams = NFTParams
   { npToken   :: !TokenName
   , npAddress :: !Address
   } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
   utxos <- utxosAt $ npAddress np
   case Map.keys utxos of
       []       -> Contract.logError @String "no utxo found"
       oref : _ -> do
           let tn      = npToken np
           let val     = Value.singleton (curSymbol oref tn) tn 1
               lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
               tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
           ledgerTx <- submitTxConstraintsWith @Void lookups tx
           void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
           Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
 where
   mint' = awaitPromise $ endpoint @"mint" mint
```

Run the test emulator Trace:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.NFT> test

Output:

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99996890
    {2d8911eaeda275b8e0c4ba484f1857435a3e5720fa9ac648fc343b57, "ABC"}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 99996890
    {dd34121ddddfd43091b6f4368b4ea1715228bfb2e65558942bf052cc, "ABC"}: 1
```


## Homework Part 1


```haskell
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PaymentPubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
```

The goal of homework 1 is to write a Mary era contract that uses deadlines and signature checks to mint a specific token ABC.
We first need to implement the mkPolicy that takes the PaymentPubKeyHash, POSIXTime and ScriptContext to produce a Boolean to check both cases in which the beneficiary has signed the transaction; as well as checking that the deadline has not passed.

```haskell
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =  -- FIX ME!
   traceIfFalse "beneficiary's signature missing" checkSig      &&
   traceIfFalse "deadline has passed"             checkDeadline
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   checkSig :: Bool
   checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

   checkDeadline :: Bool
   checkDeadline = to deadline `contains` txInfoValidRange info
```


I created && logic that checks both the signature in the checkSig function and the deadline in the checkDeadline function. This will only return true if both are true. In checkDeadline, we also want to use “to” making sure we are in the valid range.


We then need to create the policy that takes both PaymentPubKeyHash and POSIXTime as pkh and deadline respectively.

```haskell
policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode pkh
   `PlutusTx.applyCode`
   PlutusTx.liftCode deadline
```

Finally, we need to get the hash for curSymbol taking in both PaymentPubKeyHash and POSIXTime.
curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline


The final Code looks like:

```haskell
{-# INLINABLE mkPolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PaymentPubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =  -- FIX ME!
   traceIfFalse "beneficiary's signature missing" checkSig      &&
   traceIfFalse "deadline has passed"             checkDeadline
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   checkSig :: Bool
   checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

   checkDeadline :: Bool
   checkDeadline = to deadline `contains` txInfoValidRange info

policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode pkh
   `PlutusTx.applyCode`
   PlutusTx.liftCode deadline

curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline
```

The final output running the test trace looks like:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Homework1> test

Output:

Slot 00111: *** CONTRACT LOG: "deadline passed"
Slot 00111: SlotAdd Slot 112

Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 99996184
    {b995b54607b38bd87d0895eafd8d32b4b25d0c98312d1ccb0542d81a, "ABC"}: 555
```

## Homework Part 2

```haskell
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
```


The goal of homework part 2 is to mint an NFT for a given UTxO where the TokenName is a ByteString.

First we need to write the mkPolicy in which checks if the UTxO is consumed, and also if the correct amount was minted.
The "hasUTx0" function is a boolean that checks the TxOutRef to report true or false.
The checkMintedAmount will check to make sure only 1 is actually minted. We also set tn’ ==  “” since we are working with an empty Bytestring.


```haskell
{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
   traceIfFalse "UTxO not consumed"   hasUTxO           &&
   traceIfFalse "wrong amount minted" checkMintedAmount
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   hasUTxO :: Bool
   hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

   checkMintedAmount :: Bool
   checkMintedAmount = case flattenValue (txInfoMint info) of
       [(_, tn', amt)] -> tn' == "" && amt == 1
       _               -> False
```

Next we will implement the policy and curSymbol functions. Each of these will only accept the oref since we are not working with the token names.

```haskell
policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
   
curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref
```

Finally, we need to implement the mint function. The mint function passes only the address here. Since the TokenName is a bytestring, we also need to declare it as:

```haskell
let tn      = TokenName ""
```

We also only need to pass 2 arguments into the singleton and mintingPolicy function:

```haskell
let val     = Value.singleton (curSymbol oref ) tn 1
               lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
```

The mint function should then look like:

```haskell
mint :: Address -> Contract w NFTSchema Text ()
mint address =  do
   utxos <- utxosAt address
   case Map.keys utxos of
       []       -> Contract.logError @String "no utxo found"
       oref : _ -> do
           let tn      = TokenName ""
           let val     = Value.singleton (curSymbol oref ) tn 1
               lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
               tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
           ledgerTx <- submitTxConstraintsWith @Void lookups tx
           void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
           Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
 where
   mint' = awaitPromise $ endpoint @"mint" mint
```

The final code should then look like:

```haskell
{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
   traceIfFalse "UTxO not consumed"   hasUTxO           &&
   traceIfFalse "wrong amount minted" checkMintedAmount
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   hasUTxO :: Bool
   hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

   checkMintedAmount :: Bool
   checkMintedAmount = case flattenValue (txInfoMint info) of
       [(_, tn', amt)] -> tn' == "" && amt == 1
       _               -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode oref
 
curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref

type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint address =  do
   utxos <- utxosAt address
   case Map.keys utxos of
       []       -> Contract.logError @String "no utxo found"
       oref : _ -> do
           let tn      = TokenName ""
           let val     = Value.singleton (curSymbol oref ) tn 1
               lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
               tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
           ledgerTx <- submitTxConstraintsWith @Void lookups tx
           void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
           Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
 where
   mint' = awaitPromise $ endpoint @"mint" mint
```

The final output running the test trace looks like:

```
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Homework2> test

Output:

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99996914
    {ab0e3b47e0b0b01f5121ceab2b531feea584d2ed89217ab8434e06ce, ""}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 99996914
    {638de4f0870aae2a48cc73b8c07ea5ecaa28073349517f4117f4b87f, ""}: 1
```





