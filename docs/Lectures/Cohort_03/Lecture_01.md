Plutus Pioneer Program - Cohort 3
January 12, 2022

Contributed By:
Joe Totes

# Lecture 1: Building Example Code and Learning Plutus Playground

## Table of Contents
- [Lecture 1: Building Example Code and Learning Plutus Playground](#lecture-1-building-example-code-and-learning-plutus-playground)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 1](#preparation-for-lecture-1)
  - [The EUTxO Model](#the-eutxo-model)
  - [The Auction Contract in the EUTxO Model](#the-auction-contract-in-the-eutxo-model)
  - [NFT’s](#nfts)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)
## Preparation for Lecture 1
Before we can get started in lecture 1, we first must get our development environment setup. This guide will be using a fresh install of Ubuntu linux.
If you want to use linux but only have a computer with windows, you can run a virtual environment inside of Windows. A great step by step guide for how to get started can be found here:
[How to install an Ubuntu VM in Windows](https://www.google.com/url?q=https://youtu.be/x5MhydijWmc&sa=D&source=editors&ust=1644942252628903&usg=AOvVaw0y0kxJWz9VN7FAqqN8tkDT)

The haddock documentation is also a great source of information for all the public plutus libraries.  This can be found here:
[Documentation for all public Plutus Libraries](https://www.google.com/url?q=https://playground.plutus.iohkdev.io/doc/haddock/&sa=D&source=editors&ust=1644942252629960&usg=AOvVaw2eoK-uGZuqWIqtuCZt0t_H)

First, Open up the terminal to get started. We will first install the necessary dependencies first for a fresh copy of linux.

We need to install Nix and get it configured properly to use IOG’s caches. In this guide we will be doing a single user install.
Before we can install Nix, we need to make sure the version of linux you are using has curl installed. First run:
```
totinj@penguin:~$ sudo sh -c 'apt update && apt install curl'

sudo sh -c 'apt update && apt install curl'
```


Now that curl is installed, we can now install Nix. Run:
```
totinj@penguin:~$ sh <(curl -L https://nixos.org/nix/install) --no-daemon
```


```
Output:

Installation finished!  To ensure that the necessary environment

variables are set, either log in again, or type

  . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


Now to finish, we need to set the environment with the following command notice from above.
Very important here to replace “totinj” with your current linux user!!
```
totinj@penguin:~$ . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


We now need to add Input Outputs caches to greatly speed up the building process. Without this step, you might be running nix-shell for days rather than minutes! Let’s create a new config file that has the associated IOG links. Run:
```
totinj@penguin:~$ mkdir ~/.config/nix

echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf

echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.cnfig/nix/nix.conf
```



With Nix now installed and configured,  we will clone the appropriate repositories from github. We will be cloning the plutus-apps and the plutus-pioneer program.
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
Head back to  to the plutus-apps directory and update it to the  current git tag:
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
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week01]$ cabal repl
```
These will also take a long time to run the first time. If successful,  you should now be ready to start the lecture:
```haskell
Ok, one module loaded.
Prelude Week01.EnglishAuction> 
```

## The EUTxO Model
This is the transcript for the lecture video on EUTxOs by Lars Brünjes. Further information about EUTxO models can be found here: Accounting Systems for Blockchains
One of the most important things you need to understand in order to write Plutus smart contracts is the counting model that Cardano uses; and that is the so-called (E)UTxO model, which is an abbreviation for extended unspent transaction output model. The UTxO model without being extended is the one that has been introduced by Bitcoin.  But there are other models. Ethereum, for example, uses a so-called account-based model, which is what you're used to from a normal bank, where everybody has an account and each account has a balance. And if you transfer money from one account to another, then the balance gets updated accordingly, but that is not how the UTxO model works. Unspent transaction outputs are exactly what the name says. They are transaction outputs that are outputs from previous transactions that happened on the blockchain that have not yet been spent.
So let's look at an example where we have two such UTxOs, one belonging to Alice, 100 ADA and another one belonging to Bob, 50 ADA. And as an example, let's assume that Alice wants to send 10 ADA to Bob. So she creates a transaction and the transaction is something that has inputs and outputs, can be an arbitrary number of inputs and an arbitrary number of outputs. And an important thing is that you can always only use complete UTxOs as input. So, if she wants to send 10 ADA to Bob, she can't simply split herexisting 100 ADA into a 90 to 10 piece. She has to use the full 100 ADA as input. So by using the UTxO 100 ADA as input to a transaction. Alice has not spent that UTxO, so it's no longer an UTxO. It's no longer unspent, it's been spent. And now she can create outputs for a transaction. So she wants to pay 10 ADA to Bob. So one output will be 10 ADA to Bob, and then she wants her change back. So she creates a second output of 90 ADA to herself.
And so this is how, even though you always have to consume complete UTxOs, you can get your change back. So you consume the complete UTxO, but then you create an output for the change and note that in a transaction, the sum of the input values must equal the sum of the output values. So in this case, 100 ADA go in and 10 plus 90 ADA go out. This is strictly speaking, not true. There are two exceptions, the first exception is transaction fees. So in the real blockchain for each transaction, you have to pay fees. So that means that the sum of input values has to be slightly higher than the sum of output values to accommodate for the fees. And the second exception is the native tokens that we have on Cardano. So it's possible for transactions to create new tokens. In which case the outputs will be higher than the inputs or to burn tokens, in which case the inputs will be higher than the outputs. But that is a somewhat advanced topic, how to handle minting and burning of native tokens in Plutus. And we'll come back to that later in the course. So for now we only look at transactions where the sum of the input value equals the sum of the output values.
So this is a first example of a simple transaction, and we see that the effect of a transaction is to consume and spend transaction output and to produce new ones. So in this example, one UTxO has been consumed, Alice original 100 ADA UTxO, and two new ones have been created. One 90 ADA UTxO belongs to Alice and another 10 ADA UTxO belongs to Bob. It's important to note that this is the only thing that happens on an UTxO blockchain. The only thing that happens when a new transaction is added to the blockchain is that someform a UTxOs becomes spent and UTxOs appear. So in particular, nothing is ever changed, no value or any other data associated with the transaction output is ever changed. The only thing that changes by a new transaction is that some of the formerly unspent transaction outputs disappear and others are created, but the outputs themselves never change. The only thing that changes is whether they are unspent or not.
Let's do one other example, a slightly more complicated one where Alice and Bob together want to pay 55 ADA each to Charlie. So they create a transaction together. And as inputs, Alice has no choice, she only has one UTxO, so she uses that one. And Bob also doesn't have a choice because neither of his two UTxOs is large enough to cover 55 ADA. So Bob has to use both his UTxOs as input. This time we need three outputs, one the 55 plus 55 equals 110 ADA for Charlie, and the two change outputs, one for Alice's change and one for Bob's change. So Alice paid 90, so she should get 35 change and Bob paid 60. So he should get five change. One thing I haven't yet explained is under which conditions a transaction can spend a given UTxO. Obviously it wouldn't be a good idea if any transaction could spend arbitrary UTxOs, if that was the case, then Bob could spend Alice's money without her consent. So the way it works is by adding signatures to transactions, so for our first example, our transaction one, because that consumes an UTxO belonging to Alice as input. Alice's signature has to be added to the transaction. And in the second example, because there are inputs belonging to both Alice and Bob, both Alice and Bob have to sign that transaction, which incidentally is something you can't do in Daedalus. So you would have to use the Cardano CLI for complex transactions like that.
Everything I've explained so far is just about the UTxO model, not the extended UTxO model. So this is all just a simple UTxO model. And the extended part comes in when we talk about smart contracts. So in order to understand that, let's just concentrate on one consumption on a UTxO by an input. And as I just explained, the validation that decides whether the transaction this input belongs to is allowed to consume that you take so in the simple UTxO model relies on digital signatures. So in this case, Alice has to sign the transaction for this consumption of the UTxO to be valid. And now the idea of the extended UTxO model is to make this more general. So instead of just having one condition, namely that the appropriate signature is present in the transaction. We replace this by arbitrary logic, and this is where Plutus comes in. So instead of just having an address that corresponds to a public key, and that can be verified by a signature that is added to the transaction, instead we have more general addresses that are not based on public keys or the hashes of public keys, but instead contain arbitrary logic that can decide under which condition this specific UTxO can be spent by a transaction. So instead of an address going to a public key, like Alice's public key in this example, there will be an arbitrary script, a script containing arbitrary logic. And instead of the signature in the transaction, the input will justify that it is allowed to consume this output with some arbitrary piece of data that we call the redeemer. So we replace the public key address,    
Alice in our example by a script, we place a digital signature by a redeemer which is an arbitrary piece of data. Now, the next question is, what exactly does that mean? What do we mean by arbitrary logic? And in particular it's important to consider what information? What context does this script have? So there are several options. And the one indicated in this diagram is that all the script sees is the redeemer. So all the information the script has in order to decide whether it's okay for the transaction to consume this UTxO or not is looking at the redeemer. And that is the thing that Bitcoin incidentally does.So, in Bitcoin, there are smart contracts, they are just not very smart.They are called Bitcoin script and Bitcoin script works exactly like this. So there's a script on the UTxO site and a redeemer on the input side and the script gets the redeemer and can use the redeemer to decide whether it's okay to consume the UTxO or not. But that's not the only option, we can decide to give more information to the script. So, Ethereum uses a different concept. In Ethereum the script basically can see everything, the whole blockchain,the whole state of the blockchain. So that's like the opposite extreme of Bitcoin. Bitcoin the script has very little context, all it can see is the redeemer. In Ethereum the solidity scripts in Ethereum can see the complete state of the blockchain. So that enables Ethereum's scripts to be much more powerful so they can do basically everything, but it also comes with problems because the scripts are so powerful, it's also very difficult to predict what a given script will do and that opens the door to all sorts of security issues and dangerous, because it's very hard to predict for the developers of an Ethereum smart contract what can possibly happen because there are so many possibilities.
So what Cardano does is something in the middle, so it doesn't offer such a restricted view as Bitcoin, but also does not have a global view as Ethereum, but instead chooses a middle ground. So the script can see the whole blockchain, can see the state of the word blockchain, but it can't see the whole transaction that is being validated. So, in contrast to Bitcoin it can just see this one input, the redeem of this one input, but it can see that and all the other inputs of the transaction and also all the outputs of the transaction and the transaction itself, and the Plutus script can use that information to decide whether it's okay to consume this output.
Now, in this example, there's only one input, but if this transaction had more than one input, then the script would be able to see those as well. There's one last ingredient that Plutus scripts need in order to be as powerful and expressive as Ethereum scripts. And that is a so-called datum which is a piece of data that can be associated with a UTxO in addition to the value. So at a script address, like in this example, in addition to this 100 ADA value, that can be an arbitrary piece of data attached, which we call datum. And with this, we can actually mathematically prove that Plutus is at least as powerful as Ethereum, so everything, every logic you can express in Ethereum you can also express in this extended UTxO model that Cardano uses, but it has a lot of important advantages in comparison to the Ethereum model. So for example, in Plutus, it is possible to check whether a transaction will validate in your wallet before you ever sent it to the chain. So something can still go wrong, so for example, your transaction can consume an output and then when it gets to the chain, somebody else has already consumed that output.This output has already been consumed by another transaction.You can't prevent that, but in that case, your transaction will simply fail without you having to pay any fees. But if all the inputs are still there, that your transaction expects, then you can be sure that the transaction will validate and that it will have the effect that you predicted when you ran it in your wallet .
This is definitely not the case in Ethereum, in Ethereum in the time between you constructing the transaction and it being incorporated into the blockchain, a lot of stuff can happen concurrently and that's unpredictable, and that can have unpredictable effects on what will happen when your script eventually executes. So that means in Ethereum it's always possible that you have to pay gas fee for a transaction, although the transaction eventually fails with an error, and that is guaranteed not to happen in Cardano. In addition to that, it's also easier to analyze a Plutus script and to check or even proof that it is secure because you don't have to consider the whole state of the blockchain, which is unknowable. You can concentrate on this context that just consists of the spending transaction. So you have a much more limited scope and that makes it much easier to understand what a script is actually doing and what can possibly happen or what could possibly go wrong.So this is it, that's the extended UTxO model that Plutus uses.
So to recapitulate in extending the normal UTxO model, we replace public key addresses from the normal UTxO model with scripts, Plutus scripts, and instead of legitimizing the consumption of new UTxO by digital signatures, as in the simple UTxO model, arbitrary data  called redeemer is used on the input side. And we also add arbitrary custom data on the output side. And the script as context when it runs, sees the spending transaction, the transaction one, in this example. So given the redeemer and the datum and the transaction with its other inputs and outputs, the script can run arbitrary logic to decide whether it's okay for this transaction to consume the output or not. And that is how Plutus works.
One thing I haven't mentioned yet is who is responsible for providing datum, redeemer and the validator, the script that validates whether a transaction can consume an input. And the rule in Plutus is that the spending transaction has to do that whereas the producing transaction only has to provide hashes.So that means if I produce an output that sits at a script address, then this producing transaction only has to include the hash of the script and the hash of the datum that belongs to this output. But optionally, it can include the datum and the script as well, fully, but that's only optional. And if a transaction wants to consume such a script output, then that transaction,the spending transaction has to include the datum and the redeemer and the script. So that's the rule, how it works in Plutus, which of course means that in order to be able to spend a given input, you need to know the datum because only the hash is publicly visible on the blockchain. Which is sometimes a problem and not what you want and that's where this possibility comes into to also include it in the producing transaction. Otherwise only people that know the datum by some other means not by looking at the blockchain would be able to ever spend such an output.
So this is the UTxO model, the extended unspent transaction output model. And that is of course not tied to a specific programming language.I mean, what we have is Plutus, which is based on Haskell, but in principle, you could use the same concept, the same UTxO model with a completely different programming language. And we also plan to write compilers from other programming languages to Plutus script which issort of the assembly language and aligned Plutus.So there's an extended UTxO model is different from the specific programming language we use.In this course, we will use Plutus obviously, but the understanding the UTxO model is independently valid from understanding Plutus or learning Plutus syntax.

## The Auction Contract in the EUTxO Model
Code is broken down into onchain and off chain code. On chain code just checks and validates, it just says yes or no. The off chain code actively creates that translation that will then pass validation. Both of the on chain and off chain parts are uniformly written in haskell. This is largely advantageous as code can be shared, and you only need to concern yourself with one programming language.

        Looking at the auction contract EnglishAuction.hs, we see the various data types are listed first in the contract:
 
```haskell
minLovelace :: Integer
minLovelace = 2000000

data Auction = Auction
  { aSeller   :: !PaymentPubKeyHash
  , aDeadline :: !POSIXTime
  , aMinBid   :: !Integer
  , aCurrency :: !CurrencySymbol
  , aToken    :: !TokenName
  } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
  {-# INLINABLE (==) #-}
  a == b = (aSeller   a == aSeller   b) &&
           (aDeadline a == aDeadline b) &&
           (aMinBid   a == aMinBid   b) &&
           (aCurrency a == aCurrency b) &&
           (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bBidder :: !PaymentPubKeyHash
  , bBid    :: !Integer
  } deriving P.Show

instance Eq Bid where
  {-# INLINABLE (==) #-}
  b == c = (bBidder b == bBidder c) &&
           (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close
  deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum
  { adAuction    :: !Auction
  , adHighestBid :: !(Maybe Bid)
  } deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

data Auctioning
instance Scripts.ValidatorTypes Auctioning where
  type instance RedeemerType Auctioning = AuctionAction
  type instance DatumType Auctioning = AuctionDatum
```
Followed by the main on chain code (validation):
```haskell
{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
  traceIfFalse "wrong input value" correctInputValue &&
  case redeemer of
      MkBid b@Bid{..} ->
          traceIfFalse "bid too low"        (sufficientBid bBid)         &&
          traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
          traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
          traceIfFalse "wrong refund"       correctBidRefund             &&
          traceIfFalse "too late"           correctBidSlotRange
      Close           ->
          traceIfFalse "too early" correctCloseSlotRange &&
          case adHighestBid ad of
              Nothing      ->
                  traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
              Just Bid{..} ->
                  traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                  traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)

where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  input :: TxInInfo
  input =
    let
      isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
          Nothing -> False
          Just _  -> True
      xs = [i | i <- txInfoInputs info, isScriptInput i]
    in
      case xs of
          [i] -> i
          _   -> traceError "expected exactly one script input"

  inVal :: Value
  inVal = txOutValue . txInInfoResolved $ input

  auction :: Auction
  auction = adAuction ad

  tokenValue :: Value
  tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

  correctInputValue :: Bool
  correctInputValue = inVal == case adHighestBid ad of
      Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
      Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

  sufficientBid :: Integer -> Bool
  sufficientBid amount = amount >= minBid ad

  ownOutput   :: TxOut
  outputDatum :: AuctionDatum
  (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
          Nothing   -> traceError "wrong output type"
          Just h -> case findDatum h info of
              Nothing        -> traceError "datum not found"
              Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                  Just ad' -> (o, ad')
                  Nothing  -> traceError "error decoding data"
      _   -> traceError "expected exactly one continuing output"

  correctBidOutputDatum :: Bid -> Bool
  correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                            (adHighestBid outputDatum == Just b)

  correctBidOutputValue :: Integer -> Bool
  correctBidOutputValue amount =
      txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

  correctBidRefund :: Bool
  correctBidRefund = case adHighestBid ad of
      Nothing      -> True
      Just Bid{..} ->
        let
          os = [ o
               | o <- txInfoOutputs info
               , txOutAddress o == pubKeyHashAddress bBidder Nothing
               ]
        in
          case os of
              [o] -> txOutValue o == Ada.lovelaceValueOf bBid
              _   -> traceError "expected exactly one refund output"

  correctBidSlotRange :: Bool
  correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

  correctCloseSlotRange :: Bool
  correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

  getsValue :: PaymentPubKeyHash -> Value -> Bool
  getsValue h v =
    let
      [o] = [ o'
            | o' <- txInfoOutputs info
            , txOutValue o' == v
            ]
    in
      txOutAddress o == pubKeyHashAddress h Nothing
```
Next is where the compilation happens:
```haskell
typedAuctionValidator :: Scripts.TypedValidator Auctioning
typedAuctionValidator = Scripts.mkTypedValidator @Auctioning
  $$(PlutusTx.compile [|| mkAuctionValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
where
  wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction
```
After that follows with the off chain code starting with the three endpoints start, bid, and close.
```haskell
data StartParams = StartParams
  { spDeadline :: !POSIXTime
  , spMinBid   :: !Integer
  , spCurrency :: !CurrencySymbol
  , spToken    :: !TokenName
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams
  { bpCurrency :: !CurrencySymbol
  , bpToken    :: !TokenName
  , bpBid      :: !Integer
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
  { cpCurrency :: !CurrencySymbol
  , cpToken    :: !TokenName
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =
      Endpoint "start" StartParams
  .\/ Endpoint "bid"   BidParams
  .\/ Endpoint "close" CloseParams

start :: AsContractError e => StartParams -> Contract w s e ()
start StartParams{..} = do
  pkh <- ownPaymentPubKeyHash
  let a = Auction
              { aSeller   = pkh
              , aDeadline = spDeadline
              , aMinBid   = spMinBid
              , aCurrency = spCurrency
              , aToken    = spToken
              }
      d = AuctionDatum
              { adAuction    = a
              , adHighestBid = Nothing
              }
      v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints typedAuctionValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "started auction %s for token %s" (P.show a) (P.show v)

bid :: forall w s. BidParams -> Contract w s Text ()
bid BidParams{..} = do
  (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
  logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

  when (bpBid < minBid d) $
      throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
  pkh <- ownPaymentPubKeyHash
  let b  = Bid {bBidder = pkh, bBid = bpBid}
      d' = d {adHighestBid = Just b}
      v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + bpBid)
      r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

      lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                Constraints.otherScript auctionValidator                P.<>
                Constraints.unspentOutputs (Map.singleton oref o)
      tx      = case adHighestBid of
                  Nothing      -> Constraints.mustPayToTheScript d' v                            <>
                                  Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                  Constraints.mustSpendScriptOutput oref r
                  Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
                                  Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                  Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                  Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
      bpBid
      (P.show adAuction)
      (P.show bpCurrency)
      (P.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
  (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
  logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

  let t      = Value.singleton cpCurrency cpToken 1
      r      = Redeemer $ PlutusTx.toBuiltinData Close
      seller = aSeller adAuction

      lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                Constraints.otherScript auctionValidator                P.<>
                Constraints.unspentOutputs (Map.singleton oref o)
      tx      = case adHighestBid of
                  Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                  Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                  Constraints.mustSpendScriptOutput oref r
                  Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                  Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
                                  Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                  Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "closed auction %s for token (%s, %s)"
      (P.show adAuction)
      (P.show cpCurrency)
      (P.show cpToken)

findAuction :: CurrencySymbol
          -> TokenName
          -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn = do
  utxos <- utxosAt $ scriptHashAddress auctionHash
  let xs = [ (oref, o)
           | (oref, o) <- Map.toList utxos
           , Value.valueOf (_ciTxOutValue o) cs tn == 1
           ]
  case xs of
      [(oref, o)] -> case _ciTxOutDatum o of
          Left _          -> throwError "datum missing"
          Right (Datum e) -> case PlutusTx.fromBuiltinData e of
              Nothing -> throwError "datum has wrong type"
              Just d@AuctionDatum{..}
                  | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, o, d)
                  | otherwise                                           -> throwError "auction token missmatch"
      _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = awaitPromise (start' `select` bid' `select` close') >> endpoints
where
  start' = endpoint @"start" start
  bid'   = endpoint @"bid"   bid
  close' = endpoint @"close" close

mkSchemaDefinitions ''AuctionSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

```
Instead of having an unparameterized minting policy, we will change it to a parametrized one. This will instead allow an owner from a specific public key hash to mint, rather than anyone.
We can now open and load the Signed.hs minting script.
Where txSignedBy and scriptContextTxInfo are:
And the modified off chain code to account for the PaymentPubKeyHash:
```haskell
data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount    :: !Integer
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
  pkh <- Contract.ownPaymentPubKeyHash
  let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
      lookups = Constraints.mintingPolicy $ policy pkh
      tx      = Constraints.mustMintValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
where
  mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

```
Run the test emulator Trace:
```haskell
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
```haskell
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free>

:l src/Week05/NFT.hs

Output:

Ok, one module loaded.
```

The onchain code for NFT.hs looks like:
```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                        traceIfFalse "wrong amount minted" checkMintedAmount
where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  hasUTxO :: Bool
  hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

  checkMintedAmount :: Bool
  checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> tn' == tn && amt == 1
      _               -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode oref
  `PlutusTx.applyCode`
  PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
```
The offchain code for NFT.hs looks like:
```haskell
data NFTParams = NFTParams
  { npToken   :: !TokenName
  , npAddress :: !Address
  } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
  utxos <- utxosAt $ npAddress np
  case Map.keys utxos of
      []       -> Contract.logError @String "no utxo found"
      oref : _ -> do
          let tn      = npToken np
          let val     = Value.singleton (curSymbol oref tn) tn 1
              lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
              tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
where
  mint' = awaitPromise $ endpoint @"mint" mint
```
Run the test emulator Trace:
```haskell
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
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =  -- FIX ME!
  traceIfFalse "beneficiary's signature missing" checkSig      &&
  traceIfFalse "deadline has passed"             checkDeadline
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
policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode pkh
  `PlutusTx.applyCode`
  PlutusTx.liftCode deadline
```
Finally, we need to get the hash for curSymbol taking in both PaymentPubKeyHash and POSIXTime.
```haskell
curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline
```
The final Code looks like:
```haskell
{-# INLINABLE mkPolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PaymentPubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =  -- FIX ME!
  traceIfFalse "beneficiary's signature missing" checkSig      &&
  traceIfFalse "deadline has passed"             checkDeadline
where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  checkSig :: Bool
  checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

  checkDeadline :: Bool
  checkDeadline = to deadline `contains` txInfoValidRange info

policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode pkh
  `PlutusTx.applyCode`
  PlutusTx.liftCode deadline

curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline
```
The final output running the test trace looks like:
```haskell
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
The checkMintedAmount will check to make sure only 1 is actually minted. We also set tn’ ==  “” since we are working with an empty Bytestring.
```haskell
{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
  traceIfFalse "UTxO not consumed"   hasUTxO           &&
  traceIfFalse "wrong amount minted" checkMintedAmount
where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  hasUTxO :: Bool
  hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

  checkMintedAmount :: Bool
  checkMintedAmount = case flattenValue (txInfoMint info) of
     [(_, tn', amt)] -> tn' == "" && amt == 1
      _               -> False
```
Next we will implement the policy and curSymbol functions. Each of these will only accept the oref since we are not working with the token names.
```haskell
policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode oref
 
curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref
```
Finally, we need to implement the mint function. The mint function passes only the address here. Since the TokenName is a bytestring, we also need to declare it as:
```haskell
let tn      = TokenName ""
```
We also only need to pass 2 arguments into the singleton and mintingPolicy function:
```haskell
let val     = Value.singleton (curSymbol oref ) tn 1
              lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
```
The mint function should then look like:
```haskell
mint :: Address -> Contract w NFTSchema Text ()
mint address =  do
  utxos <- utxosAt address
  case Map.keys utxos of
      []       -> Contract.logError @String "no utxo found"
      oref : _ -> do
          let tn      = TokenName ""
          let val     = Value.singleton (curSymbol oref ) tn 1
              lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
              tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
where
  mint' = awaitPromise $ endpoint @"mint" mint
```
The final code should then look like:
```haskell
{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx =
  traceIfFalse "UTxO not consumed"   hasUTxO           &&
  traceIfFalse "wrong amount minted" checkMintedAmount
where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  hasUTxO :: Bool
  hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

  checkMintedAmount :: Bool
  checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> tn' == "" && amt == 1
      _               -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol oref = scriptCurrencySymbol $ policy oref

type NFTSchema = Endpoint "mint" Address

mint :: Address -> Contract w NFTSchema Text ()
mint address =  do
  utxos <- utxosAt address
  case Map.keys utxos of
      []       -> Contract.logError @String "no utxo found"
      oref : _ -> do
          let tn      = TokenName ""
          let val     = Value.singleton (curSymbol oref ) tn 1
              lookups = Constraints.mintingPolicy (policy oref ) <> Constraints.unspentOutputs utxos
              tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
where
  mint' = awaitPromise $ endpoint @"mint" mint
```
The final output running the test trace looks like:
```haskell
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Homework2> test

Output:

Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58:

    {, ""}: 99996914

    {ab0e3b47e0b0b01f5121ceab2b531feea584d2ed89217ab8434e06ce, ""}: 1

Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491:

    {, ""}: 99996914

    {638de4f0870aae2a48cc73b8c07ea5ecaa28073349517f4117f4b87f, ""}: 1
```