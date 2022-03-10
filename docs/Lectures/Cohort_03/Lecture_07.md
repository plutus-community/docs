Plutus Pioneer Program - Cohort 3 
March 2, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture7](https://youtu.be/CLOHdIGgy90)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1gqetrtmtsdAg0-kOYF94qlN10UG74syDLgSIzRBGg8Q/edit#)

# Lecture 7: State Machines

## Table of Contents

- [Lecture 7: State Machines](#lecture-7-state-machines)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 7](#preparation-for-lecture-7)
  - [Introduction](#introduction)
  - [Commit Schemes](#commit-schemes)
  - [Implementation without State Machines](#implementation-without-state-machines)
  - [State Machines](#state-machines)
  - [Homework](#homework)

## Preparation for Lecture 7

Before we can get started in lecture 7, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 7 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week07 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week07$ cat cabal.project
```

Grab the plutus-apps tag inside the cabal.project file:

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:13836ecf59649ca522471417b07fb095556eb981
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 13836ecf59649ca522471417b07fb095556eb981
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week07 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week07]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week07]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week07]$ cabal repl
```

If successful,  you should now see in the terminal:

```haskell
Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - plutus-pioneer-program-week07-0.1.0.0 (lib) (ephemeral targets)
Preprocessing library for plutus-pioneer-program-week07-0.1.0.0..
GHCi, version 8.10.4.20210212: https://www.haskell.org/ghc/  :? for help
[1 of 4] Compiling Week07.EvenOdd   ( src/Week07/EvenOdd.hs, /home/totinj/plutus-pioneer-program/code/week07/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-pioneer-program-week07-0.1.0.0/build/Week07/EvenOdd.o )
[2 of 4] Compiling Week07.StateMachine ( src/Week07/StateMachine.hs, /home/totinj/plutus-pioneer-program/code/week07/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-pioneer-program-week07-0.1.0.0/build/Week07/StateMachine.o )
[3 of 4] Compiling Week07.TestEvenOdd ( src/Week07/TestEvenOdd.hs, /home/totinj/plutus-pioneer-program/code/week07/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-pioneer-program-week07-0.1.0.0/build/Week07/TestEvenOdd.o )
[4 of 4] Compiling Week07.TestStateMachine ( src/Week07/TestStateMachine.hs, /home/totinj/plutus-pioneer-program/code/week07/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-pioneer-program-week07-0.1.0.0/build/Week07/TestStateMachine.o )
Ok, four modules loaded.
Prelude Week07.EvenOdd>
```

We can now begin with the lecture.

## Introduction


In today's lecture, we are introduced to state machines. State machines can be very useful to write shorter and more concise contracts both on-chain and off-chain. There is support for state machines in the Plutus libraries that is higher level and builds on top of the lower level mechanisms we have seen so far. 

However, it should also be mentioned that at the moment, there is a certain overhead with using state machines. If you write the same contract with the state machine instead of without, then it will require more resources to run. For that reason, they have not seen much use in practice yet.

The Plutus team is continuously working on improving performance and optimizing both the compiler and the interpreter. We can expect state machines to be really useful in the near future. They will make writing on-chain and off-chain code much easier.


## Commit Schemes



Looking at an example for today, we want to implement a game played between Alice and Bob. It is a bit like rock paper scissors, but even simpler as there are only two options.

![7 1](https://user-images.githubusercontent.com/59018247/157315544-93d78210-e44e-4e63-98da-a3a014557419.jpg)

Alice and Bob both have two options, they can either play 0 or 1. So, at the same time, they raise their hand and there is one gesture for 0 and one for 1. Depending on what they play, one of them wins. 

![7 2](https://user-images.githubusercontent.com/59018247/157315578-ae0c8c94-efa2-418c-beb0-ce29e5caf70e.jpg)

- If they both use the same gesture, both choose 0 or both choose 1, then Alice wins. 

- If the choices are different, then Bob wins.

Now, let's imagine that Alice and Bob can not meet in person, but they still want to play the game. They decide to play it via email, but how could that work? 

![7 3](https://user-images.githubusercontent.com/59018247/157315602-2e282649-eb40-4101-a0bc-f3651f933bec.jpg)

Alice can send an email with her choice, let's say 0 to Bob. However, this of course gives a very unfair advantage to Bob. Bob now opens Alice's mail and sees she picked 0. Therefore he can simply reply by sending 1 and he wins. Alice can also instead pick 1. However , Bob can simply choose 0.

![7 4](https://user-images.githubusercontent.com/59018247/157315640-854dab7b-5978-44c7-87bc-f27b385d1e5c.jpg)

In this case Bob would always win, at least if he is unfair. So, what can we do about that to make it fair? There is a very clever trick that is often used in cryptographic protocols called commit schemes. The idea is that Alice does not reveal her choice to Bob, but she commits to her choice so that she later can not change her mind. 

One possible way to make this work is by using hash functions. Hash functions in other words, are one way functions. It is difficult given a hash to reconstruct the original document or the original byte string that was hashed to this hash. 

![7 5](https://user-images.githubusercontent.com/59018247/157315672-d91bb891-fedc-431f-9a1f-40f68c454eb5.jpg)

Looking at the game, instead of sending her choice to Bob, she instead sends the hash of her choice to Bob. In this example she would send the hash of 0 to Bob which is just a cryptic byte string. So now Bob sees this cryptic byte string and he has no idea whether Alice picked, 0 or 1. Let’s say that he chooses to pick 0. There would be no need for him to use a hash, so he can just send the 0 in plain text. 

![7 6](https://user-images.githubusercontent.com/59018247/157315749-478e41dc-21a7-4b5a-a6f2-118399264dec.jpg)

In this case Alice would have won, however Bob still has no proof that Alice has won. There is then one additional step, that Alice has to send another message to Bob  with her actual choice. Bob can then check whether the hash of Alice's claim choice is indeed the hash he received earlier. And if it is, then he knows that:

- Alice is not lying and that she picked 0, resulting in him losing.

- If this hash does not match up, then he knows Alice is cheating or trying to cheat and he would win.

- If Alice instead picked 1, that would work exactly the same except that the hash would be different.

This sounds promising, but there is one big problem with this method. In this game, there are only two choices, 0 and 1. So there are only two possible hashes, which may look very cryptic to Bob the first time they play. However, sooner or later he will notice that he always sees one of these two hashes and then he knows which choice Alice made. 

![7 7](https://user-images.githubusercontent.com/59018247/157315783-1ff391e2-f6cd-4dd1-9495-cf725c9ca7f5.jpg)

Instead of sending the hash of 0, she can first concatenate 0 with some arbitrary byte string that she picks. So the way this would work is first Alice picks a nonce, some arbitrary text. She sends the hash of the nonce and her choice, let’s say 0 for example to Bob. This would be some other cryptic byte string. This time however, it is not always the same byte string. Bob receives that and then it proceeds as before. He would send his choice, for example 0. In the third message, Alice has to not only reveal her original choice, but she has to send the nonce as well. So in this case, she would send the nonce and 0. Bob would then check that the hash of Alice's claimed nonce concatenated with 0 is indeed the hash he originally received. 

- If it is, he knows he lost.

- If it is not, then he knows she tried to cheat him.

This works very nicely and this is what we will try to implement in Plutus on Cardano. First we will use what we have seen so far. Second, we will see how by using state machines, the code can be much clearer and much shorter. Now, what will this look like?

![7 8](https://user-images.githubusercontent.com/59018247/157315848-9d8d8ce3-41af-4a33-b338-174f405e75c7.jpg)

-  First Alice opens the game by posting the hash of her nonce combined with the choice she makes to play, so we have the hash.

- Then if Bob plays along, he will post his own choice, so Bob will play. Now we have the hash and Bob's choice, let's call it C Bob.

- If at this point, Alice realizes that she has won depending on Bob's choice, she can reveal her secret and the game ends with her winning. 

Let's assume that when both Alice and Bob play, they put down a certain amount of money. In this scenario: 

- If Alice has won, Alice gets Bob's claim and her own back.

- If however, after Bob makes his move, Alice sees that she has lost, there's no need actually for her to do anything; because she has lost anyway.

- After a certain deadline has been reached and Alice just does not reply, then Bob claims his win.

- One last option that we haven't considered, that after Alice starts playing, Bob simply is not interested and doesn't reply. So, in that case, there must be a way for Alice to get her own money back. 

These are basically all the things that can happen. The different stages of the game starts by Alice opening with the hash of the concatenation of her nonce and her move. Then if Bob does not reply, Alice gets her money back. If Bob does reply, he puts down his choice. Then there are two possibilities here. 

- Alice realizes she has won and she needs to prove it to Bob by revealing and she wins.
- Alice does not do anything and after some time has passed. Bob can claim the win and he gets both Alice's and his own stake.

So let's implement this in Plutus now. First, just using the techniques we already know about.


## Implementation without State Machines



Let's look at the implementation for this game. We will first look at the file EvenOdd.hs.

The data type game is used as a parameter for the contract.


```haskell
data Game = Game
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
```

- ```gFirst``` and ```gSecond``` are the two players identified by their public key hashes.

- ```gStake``` is an integer that denotes the number of lovelace that are to be used as stake in the game by each player.

- ```gPlayDeadline``` is by what time the second player has to make a move before the first player can claim back his stake.

-  ```gRevealDeadline``` is in the case that the second player has made a move, how much time the first player has to claim victory by revealing his nonce.

- ```gToken``` is a record type of type asset class. This is an important and common technique that is used in a context like this. 


So, often we have a situation where we have state in some form. In our case it is the game and as we saw in the diagram earlier, there are a couple of states:


- The first player opens, the state would be the hash of the nonce concatenated with the choice of the first player.

- If the second player replies, the state would be extended by also including the choice of the second player.

In blockchain like ethereum where we have mutable state in the contracts, we would simply update the state of the contract in place. 

In Cardano everything is immutable, so nothing can ever change. UTxOs are either unspent or they get spent by a transaction and new UTxOs get created. We are never able to change an existing UTxO. 

However, that does not mean that we can not do things like this game where we have a changing state. We just use a technique that is also very frequently used in Haskell. Instead of changing the state in place, we simply create a new state. In the case of the Cardano blockchain, that means we spend the UTxO and create a new UTxO. This would then contain the extended datum or the change datum.

One problem that arises is how do we know that this new UTxO corresponds to the old one? We somehow must link the two together so that there is a continuity between the two. This would allow us to see and represent the state change.

One proper solution is to put an NFT into the UTxO. In the case of this game example, the stake that the players put in can be there and in addition to that we have this NFT. 

When the first player opens and creates the UTxO, this player also has to add this NFT to the value. When the state changes, so in our example that happens when Bob makes his choice. Bob will create a transaction that consumes the existing UTxO, then create a new one with the updated datum while still containing the same NFT. This NFT is exactly what links these two UTxOs together. 

In order to find the current state of the game, we must just look for UTxO at the correct address, the address given by the validator script for the game containing this NFT. Then we know we have found the right instance of the game, and the right UTxO. 

This is why we need this last field here, the gToken. That will be the asset class of the NFT that we use to identify the correct UTxO .This can also be referred to as  a stake token, if an NFT is used for this reason.

There is an additional reason for using an NFT, then just linking old and new state together. In the case of this game, anybody could create a UTxO with exactly the same datum as the first player. So there is no way to prevent somebody from trying to disrupt the game by creating one or more UTxOs at the same address with exactly the same datum . 

This is the reason why we use an NFT as the stake token and not an arbitrary native token. An NFT by definition is unique and only exists once. Anyone can produce a UTxO that contains this datum, but only one of these UTxOs can actually contain the NFT as part of its value. There can be at most one UTxO sitting at the address with the correct datum and containing the NFT that corresponds to the gToken field given as part of the datum.


**GameChoice:**

```haskell
data GameChoice = Zero | One

deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)
```

- These are two moves the players can make, zero or one.

- We derive equality in the normal Haskell sense and an ord instance; unfortunately for the Plutus equivalence of eq and ord, that's not possible.


We need ```Eq```, for Plutus ```Eq```, so we do that by hand in the usual way:

```haskell
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False
```

- Zero equals zero 
- One equals one  
- All other combinations are not equal.

For this to work with template Haskell, we have to put this inlineable pragma here as well for the equals operation.

The GameDatum is what we will use as state for the contract:

```haskell
data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
```
- BuiltinByteString is the hash that the first player submits 
- ```Maybe GameChoice``` is the move by the second player 

We also need Plutus equality for ```GameDatum```. The obvious one is that two are equal if both components, the hash and the ```Maybe GameChoice```, are in fact equal.

Looking at the redeemer:

```haskell
data GameRedeemer = Play GameChoice | Reveal BuiltinByteString | ClaimFirst | ClaimSecond
    deriving Show
```
We implemented a custom type that we call ```GameRedeemer```, and that corresponds to the transitions we saw in the diagram

- ```Play``` is when the second player moves and as an argument it has a ```GameChoice```. The second player can play zero or one, so that will be play zero or play one 
- ```Reveal``` is for the case when the first player has one and must prove that by revealing its nonce so we use a byte string argument 
- ```ClaimFirst``` is the case when the second player does not make a move; so the first player can claim back his stake 
- ```ClaimSecond``` is for the case that the first player does not reveal his choice because he knows he has lost; so the second player can get his winnings 

Then we have two helper functions:

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe Datum -> Maybe GameDatum
gameDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d
```

- One called ```lovelaces``` which, given the value, extracts the amount of lovelaces contained in that value. There is a function ```fromValue``` that, given the value, extracts the ADA. This however is not an integer, it is a new typewrapper around the integer. We get to the underlying integer with this ```getLovelace``` function 
- The second one given a ```Maybe Datum```, tries to deserialize if it is a just to ```Maybe GameDatum```. We write this in the maybe monad, which means if the ```md``` is already nothing, it will return nothing. If it is just ```Datum ```d then we try to deserialize that into a ```GameDatum``` 


Now looking at the core logic of the ```mkGameValidator``` function:

```haskell
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _ -> False
```

```haskell
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
```

- The first argument here is the parameter of the game type 
- The second and the third argument we just need them due to the fact that it is not possible to use string literals to get byte strings in Haskell. This is compiled to plutus core and we want string literals representing the zero choice and the one choice. The result will just be the byte string with the digit zero and one but because we cannot use string literals we pass them in as auxiliary arguments
- Then the datum, redeemer, and context 

Before we look at this main logic, let's first look at a couple of helper functions: 

```haskell
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    outputDatum :: GameDatum
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1
```


**Info:**

```haskell
  info :: TxInfo
    info = scriptContextTxInfo ctx
```

- info extracts the TxInfo from the context 

**ownInput:**

```haskell
 ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i
```
- ownInput uses a function findOwnInput
- We first check whether we get nothing. That would be the case if we are not validating, so we trace an error. If we are validating we get this txin info i,  and then we extract the txOut from that. 

**findOwnInput**

```haskell
findOwnInput :: ScriptContext -> Maybe TxInInfo
Find the input currently being validated.
```

**TxInInfo**

```haskell
data TxInInfo
An input of a pending transaction.
Constructors
TxInInfo
 
txInInfoOutRef :: TxOutRef
txInInfoResolved :: TxOut
```

**ownOutput:**

```haskell
ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"
```

- ```getContinuingOutputs``` given a script context returns a list of ```txOuts```, and those are the outputs that sit at the script address we are currently validating. 

```haskell
getContinuingOutputs :: ScriptContext -> [TxOut]
```

- In our case we expect there to be exactly one such output, so we just pattern match against the list that we get as a result. 
- If that contains exactly one element that is the output we are interested in
- Otherwise we trace an error 

**outputDatum:**

```haskell
 outputDatum :: GameDatum
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "game output datum not found"
        Just d  -> d
```



- this should give us the datum of type ```GameDatum``` of our ```ownOutput```
- we are using this hyperfunction ```gameDatum``` we looked at earlier and recall that takes a maybe datum and then returns a ```Maybe GameDatum``` 
- given our ```ownOutput``` we now have the datum hash of that output 
- we insist that the datum is actually included and there's a function ```findDatum```

**findDatum:**

```haskell
findDatum :: DatumHash -> TxInfo -> Maybe Datum
Find the data corresponding to a data hash, if there is one
```

- we use flip here where flip is a standard Haskell function that switches the two arguments of a two argument function
- we saw that info was actually the second argument, but by using flip we make it the first so then the second one is the datum hash 
- the bind is then used and if all goes well we get maybe datum and then we use our helper function to turn that into a ```gameDatum```
- all of this can fail, but if it does, we trace an error and otherwise we return this ```gameDatum``` 

**checkNonce:**

```haskell
   checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'
```


- ```checkNonce``` is for the case that the first player has won and wants to prove it by revealing his nonce. Followed by proving that the hash he submitted in the beginning of the game fits this nonce.

- the first argument is the hash he submitted
- the second argument is the nonce he now reveals 
- the third argument is the move that both players made

In order to do this check how did the computation of the hash work where we take the nonce and concatenate it with the choice, but the choice here is of some abstract data type ```GameChoice```. In order to concatenate with the nonce we need it in byte string form and the idea is we use:

- ```bsZero``` for the zero choice 
- ```bsOne``` for the one choice 

This is just this conversion and we call the byte string ```cFirst```, so ```cSecond``` is of type ```GameChoice```. Therefore, if the choice was zero we take the byte string representing zero (if it was one we would take the bytestring representing one).

Then to the hash we take the nonce concatenated with this byte string and apply the ```sha2_256``` hash function to it. Then the check is to make sure that that is the hash the first player committed in the first place 

**nftToFirst:**

```haskell
nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1
```

We now have this state token NFT that identifies the correct UTxO and the question is what happens to this NFT after the game is over and there is no UTxO at the game address anymore. It seems reasonable that the NFT should return back to the first player regardless of the player that won the game

There is a function called ```valuePaidTo``` and gets the context or the info from the context and ```unPaymentPubKeyHash```. It then adds up all the values that go to that pub key hash in some output of the transaction. In other words, this just means that the first player gets the token. 

**Side Note** 

*If we recall that an address has two parts, a payment part and the staking part. The staking part could benefit a malicious player in this case. The staking rewards occurred by the UTxO could be paid to somebody else, so a malicious player could in this step here pay back the NFT to you. However,  they can use an address where the payment part belongs to you but the staking rewards go to himself. The same is true for the winnings of the game, so one of the players wins. We will make sure that the winnings go to the winner but what could also happen is that the loser basically does pay the winnings to the winner, but instead uses an address where the payment part belongs to the winner but the staking part belongs to himself. He would indeed pay the winner the winnings but as long as the winner leaves it at this address, he himself the loser would still receive the staking rewards for this.* 


So now we can look at the conditions:

```haskell
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
```

There is one condition that applies to all cases simultaneously:

- the input we are validating must be identified by the state token.


**The first situation is:**

```haskell
(GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)
```

This is where the first player has moved. The second player is moving now, so this is the transaction where the second player moves and chooses to move ```c```. 

- the datum, the second component must be nothing. Remember that just means that the second player hasn't moved to is moving now.

In this case, what do I have to check?

- First, we check that this move is indeed made by the second player.
- Second, he has to sign the transaction. T
- Third, we check that the first player has put down the stake for the game.
- Fourth, we check that now in this transaction, the second player has put down his own stake. So in the output, we now have twice the G stake game.
- Fifth, we then know exactly what the datum of the output must be. It must be the same hash as before, but now the nothing is replaced by just c where c is exactly the move; the move the second player is making.
- Sixth, the move has to happen before the first deadline. That is exactly the point of this first step, by the play deadline until then the move must have happened.
- Lastly, the NFT must be passed onto the new UTxO to identify that again.

**The second situation is:**

```haskell
(GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst
```

The second situation is both players have moved and the first player discovers that he has won. In order to prove that and get the winnings, he has to reveal his nonce.

- First, it has to be signed by the first player.
- Second, the nonce must agree with the hash he submitted earlier.
- Third, he must do this before the reveal deadline.
- Fourth, the input must contain the stake of both players.
- Lastly, the NFT must go back to the first player.


**The third situation is:**

```haskell
(GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst
```

In this case, the second player has not moved yet and also does not move in the deadline. Therefore, the first player wants his stake back.

- First, this must be signed by the first player.
- Second, it must only happen after the deadline has passed.
- Third, making sure the first player has provided his stake 
- Lastly, we must get the NFT back to the first player

**The fourth situation is:**

```haskell
(GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst
```

In the last case, both players have moved, but the first player has realized that he did not win and therefore did not reveal his nonce or he got disconnected. In any case, he missed the deadline so the second player wins. 

- First, the second player must sign this transaction.
- Second, it must not happen before the deadline. He can only do that once the reveal deadline has passed to give the first player enough time to reveal the nonce if the first player won.
- Third, we check again that the consumed UTxO has the correct stake.
- Lastly, both players must have provided their stake. Although the second player has won and receives the money, the NFT must go back to the first player.

Those four cases are all situations in which we can have.

```haskell
_ -> False
```

So in all other cases, we fail validation.


So now let's look at the rest of the on-chain code:


```haskell
data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator
```

Now, as preparation for the off-chain code, we will always need to find the right UTxO. We write this helper function ```findGameOutput```:

```haskell
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, GameDatum))
findGameOutput game = do
    utxos <- utxosAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (gToken game) == 1
```

 This function gets the game, and then in the contract monad tries to find the UTxO. This could fail, so we use maybe, and then we return the reference and the game datum. 

- Using this find function, after we have all the UTxOs turn them into a list of pairs. Then as f we take such a pair, ignore the reference, and just take the ```o```. Then check whether this output contains our token.

Now there's a second helper function, ```waitUntilTimeHasPassed```:

```haskell
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2
```

The purpose of this is that it receives a ```POSIXTime```, and then it waits until that ```POSIXTime``` has passed such that we are in the next slot.

- We start by getting the current slot, then log it. Then we use something provided by the contract monad called ```awaitTime``` which gets a ```POSIXtime``` and blocks the contract until the time has come. 
- To make sure that we are in the next slot, we wait for one slot using ```waitNslots```.
- Then ask for the current slot and also log it.

Okay, so now we have two contracts for the two players. One for the first player to play the game, and one for the second player. These correspond to the parameters first params and second params respectively.

```haskell
data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
```

So, first params we don't need the first player. The first player will be the owner of the wallet that invokes this contract, but we need the second. 

- First, we need the second player
- Second, we need the stake
- Third, the play deadline
- Fourth, the reveal deadline
- Fifth,  we need the nonce that the first player wants to use to conceal his choice.
- Sixth and seventh, we need the NFT, which we split into currency symbol and token name.
- Lastly, the choice, the move that the player wants to make.

Looking at contract:

```haskell
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        bs   = sha2_256 $ fpNonce fp `appendByteString` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    waitUntilTimeHasPassed $ fpPlayDeadline fp
```

- First get our own public key hash.
- Then we can define the value of the game type.So we put our own public key hash as first player and then use the parameters we got from the ```FirstParams```. For the token we just assemble the currency symbol and token name into an asset class.
- The ```v``` value is just our stake plus the NFT that we must put into the UTxO.
- ```c``` is our choice.
- ```bs``` we compute the hash, our commitment to our choice. We take the nonce and concatenate it (if you want to play) with zero. There's zero byteString, and otherwise the one byteString and hash the results. 
- ```tx```, the constraints for the transaction are very simple. We must produce a script output at this address with the datum that contains the hash. We wait for the transaction and log a message.

So now, the second player has a chance to move, but it must happen before this play deadline. We then wait until this deadline has passed, which results in several cases.


```haskell
  m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do

                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"
```

We check whether we find UTxO containing the NFT, if we don't find it, then something is wrong.

In this case, the second player hasn't moved. 

- The deadline has passed, the second player has not moved.
- What we can do is we can invoke this ```ClaimFirst``` redeemer to get our stake back.
- As constraints, we must spend this UTxO we found with this redeemer and as lookups we need to provide the UTxO. We also must provide the validator of the game.
- In that case, we log that we reclaimed the stake.

Second case is that the second player did move and they are then in turn again. Two cases that the second player moved and won or that the second player moved and lost.

```haskell
 GameDatum _ (Just c') | c' == c -> do
```

This is the case where the second player choose the same move that we played, so we won.

```haskell
logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"
```

In the last case, the second player won but we can not do anything so we do not do anything. But if we won, we must now reveal our nonce to get the winning.

- We use the reveal nonce redeemer.
- Then we must also submit this transaction before the deadline for revealing has passed. We again need lookups and need to specify the UTxO while providing the ```gameValidator```.
- Finally, we submit and wait and have won.


Now for the second player:

```haskell
data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
```


The parameters are very similar, except that now we don't need to provide the second player because that is us. We do not need the nonce now because the nonce was only for the first player.

```haskell
secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
```

We start by looking up our own public key hash, and can then define the game value similar to what we did for the first player.

```haskell
m <- findGameOutput game
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            waitUntilTimeHasPassed $ spRevealDeadline sp
```

Then, we try to find the UTxO that contains the NFT. If we found the game and now we want to make our move, so invoke the play redeemer.

- The token is the NFT
- ```v``` is the value that we must put in the new output. Remember if we do the play transaction, we must consume the existing UTxO and while producing one at the same address. Therefore, the old one should contain the stake that the first player put in. Now we must add our own stake and keep the NFT in there. ```x``` is a local variable here to, the stake in lovelace. So we must put twice the stake and the NFT in the output.
- ```c``` is our choice

Let's look at the constraints first:

- We must spend the existing UTxO with redeemer Play, our choice.
- Then we must create a new UTxO with the updated datum. The same ```bs```, but now just ```c```, just our move. Also using ```v``` we computed, which now also contains our own stake in addition to the first player stake and the NFT.
- We must do this before the deadline passes to make the play move.

Lastly, we submit and wait for confirmation, and then log. Now it's the first player's turn again, so we wait until this reveal deadline has passed.

```haskell
m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   Constraints.mustPayToPubKey (spFirst sp) (token <> adaValueOf (getAda minAdaTxOut))
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"
```

We try to find the UTxO which will now be a different one. That is what is called ```m’```. If we do not find an UTxO any more, that means that in the meantime, while were waiting, the first player revealed and won.

So, in that case we have lost, we can't do anything. If we still do find the UTxO, it means the first player didn't reveal, which means either:

- The first player left the game for whatever reason
- The first player lost and could not reveal.

In any case now we can claim the winning. So we must spend the UTxO we found. We must do this before and after the reveal deadline has passed, because until then the first player still would have time to, to make his revelation. Therefore, we must hand back the NFT to the first player.

There's one extra thing we have to take care of here, namely we cannot just send the NFT back to the first player.Because there is this min ADA rule that every UTxO has to contain a minimal amount of ADA. Therefore, we have to add some ada to that.

For the lookups, we again need the UTxO and the validator. Finally, we can submit the transaction and log that the second player won.

```haskell
type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame
```
At the end of the file, we define the schema. Starting with two endpoints, one for the first and second player, called first and second respectively.

Then in the usual way we assemble the two contracts into one contract that we call endpoints. So as before, we give the choice between first and second. Then we block until one of the choices is made with ```awaitPromise```. It is implemented by just using the contracts we just defined, first game and second game. Finally, we recursively call endpoints again.

We can now test this contract using the emulator trace using file TestEvenOdd.hs:

```haskell

test :: IO ()
test = do
    test' Zero Zero
    test' Zero One
    test' One Zero
    test' One One

w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO' def emCfg $ myTrace c1 c2
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (gameTokenCurrency, gameTokenName)) 1)
                    , (w2, v)
                    ]
                }

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

gameTokenCurrency :: CurrencySymbol
gameTokenCurrency = "ff"

gameTokenName :: TokenName
gameTokenName = "STATE TOKEN"

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let pkh1      = mockWalletPaymentPubKeyHash w1
        pkh2      = mockWalletPaymentPubKeyHash w2
        stake     = 100_000_000
        deadline1 = slotToBeginPOSIXTime def 5
        deadline2 = slotToBeginPOSIXTime def 10

        fp = FirstParams
                { fpSecond         = pkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"
                , fpCurrency       = gameTokenCurrency
                , fpTokenName      = gameTokenName
                , fpChoice         = c1
                }
        sp = SecondParams
                { spFirst          = pkh1
                , spStake          = stake
                , spPlayDeadline   = deadline1
                , spRevealDeadline = deadline2
                , spCurrency       = gameTokenCurrency
                , spTokenName      = gameTokenName
                , spChoice         = c2
                }

    callEndpoint @"first" h1 fp

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp

    void $ Emulator.waitNSlots 10
```


So here's this ```test’``` function where:

- we have this combined function that just calls test prime for all four possible choices.
- we defined the two wallets, w1 and w2

And we use this variant of run emulator trace IO where we  can specify both players to have 1000 ADA. We also  want the first player to hold the NFT in the beginning.

Load the TestEvenOdd.hs file in the repl:

```haskell
Prelude Week07.EvenOdd> :l src/Week07/TestEvenOdd.hs

Output:
Ok, two modules loaded.
```

Testing the first case:

```haskell
Prelude Week07.EvenOdd> test' Zero Zero

Output:

Final balances
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58:
	{, ""}: 899992070
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491:
	{ff, "STATE TOKEN"}: 1
	{, ""}: 1099992060
```


We see the first player (one with NFT) won, as expected.


Testing the second case:

```haskell
Prelude Week07.EvenOdd> test' Zero One

Output:

Final balances
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58:
	{, ""}: 1097984140
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491:
	{, ""}: 901999990
	{ff, "STATE TOKEN"}: 1
```

We see the second player (one without NFT) won, as expected.

## State Machines

So, what is a state machine? A state machine, normally it has nothing to do with blockchain in particular. 

It is a system you start in a certain state, and then there are one or more transitions to other states. There may also be some states that are special, in that they are so-called final states, meaning there are no possible ways out. There are no transitions that lead out of the final state.
            
![7 8](https://user-images.githubusercontent.com/59018247/157318615-c6bb3a0d-9b97-4cf5-aefa-999f91234f89.jpg)

If we look again at the diagram we had earlier for how our game works, then we can consider that a state machine. The initial state would be where the first player has made the move. The state is basically characterized by the state owned by the hash. In this state, there are two possible transitions:

- First, where the second player plays.
- Second, where the second player does not play and the first player can reclaim.

All the nodes in this diagram correspond to states. Therefore, the errors, the edges of the graph; correspond to transitions. In the blockchain, the state machine will be represented by UTxO sitting at the state machine script address. The state of the state machine would be the datum of the UTxO and the transition will be a transaction that consumes the current state. Finally, using a trend redeemer that would then characterize the transition and then produce a new UTxO at the same address, where the datum now reflects the new state. 

Looking at the state machine documentation:
 
**StateMachineClient**

```haskell
data StateMachineClient s i

Client-side definition of a state machine.

Constructors

StateMachineClient
 
scInstance :: StateMachineInstance s i

The instance of the state machine, defining the machine's transitions, its final states and its check function.
scChooser :: [OnChainState s i] -> Either SMContractError (OnChainState s i)

A function that chooses the relevant on-chain state, given a list of all potential on-chain states found at the contract address.
To write your contract as a state machine you need * Two types state and input for the state and inputs of the machine * A 'SM.StateMachineInstance state input' describing the transitions and checks of the state machine (this is the on-chain code) * A 'StateMachineClient state input' with the state machine instance and an allocation function
In many cases it is enough to define the transition function t :: (state, Value) -> input -> Maybe (TxConstraints state) and use mkStateMachine and mkStateMachineClient to get the client. You can then use runInitialise and runStep to initialise and transition the state machine. runStep gets the current state from the utxo set and makes the transition to the next state using the given input and taking care of all payments.
```



**StateMachine**

```haskell
data StateMachine s i

Specification of a state machine, consisting of a transition function that determines the next state from the current state and an input, and a checking function that checks the validity of the transition in the context of the current transaction.

Constructors

StateMachine
 
smTransition :: State s -> i -> Maybe (TxConstraints Void Void, State s)
The transition function of the state machine. Nothing indicates an invalid transition from the current state.

smFinal :: s -> Bool

Check whether a state is the final state

smCheck :: s -> i -> ScriptContext -> Bool

The condition checking function. Can be used to perform checks on the pending transaction that aren't covered by the constraints. smCheck is always run in addition to checking the constraints, so the default implementation always returns true.

smThreadToken :: Maybe ThreadToken

The ThreadToken that identifies the contract instance. Make one with getThreadToken and pass it on to mkStateMachine. Initialising the machine will then mint a thread token value.
```

**State**

```haskell
data State s

Constructors

State
 
stateData :: s
stateValue :: Value
```

**ThreadToken**

```haskell
data ThreadToken

Constructors

ThreadToken
 
ttOutRef :: TxOutRef
ttCurrencySymbol :: CurrencySymbol
```


We can now look at a new file StateMachines.hs, to see the same game we saw earlier but now implemented using state machines.

```haskell
data Game = Game
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)
```


The only difference is earlier the token was of type asset class because we had to pass an NFT. Now that we're using state machines, we use type thread token instead.

```haskell
data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice) | Finished
    deriving Show
```

Game datum has slightly changed because we have added a second constructor that we called finished. This is supposed to represent the final state of our state machine. It will not correspond to a UTxO, but we need that for the state machine mechanism to work.

```haskell
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False
```


This makes the definition of equality slightly more complicated because now we have to take Finished into account as well. 

```haskell
{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing
```


Now, this is now the transition function of the state machine, which corresponds to the ```mkGameValidator``` that we created earlier. 

And so it takes:

- The game and now as we saw in the definition of state machine,  the state datum. This is a pair basically consisting of the datum and the value.
- The redeemer 
- Then we must return nothing if it's not allowed, and a pair of new state and constraints on the transaction.

Now let's try to compare the transition function of the state machine to the make game ```mkGameValidator``` of our first version of the game.

```haskell
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _ -> False
```

- Both functions try to determine whether a combination of datum, redeemer, and transaction are valid.
- The first difference we notice is here in our old version, we first had to check that the UTxO we're consuming actually carries the NFT, and that is missing in the new implementation because the state machine automatically takes care of that. 

Let's look at this first case where the first player moved. The component for the second player was still nothing. Therefore, now the second player wants to make a play with choice ```c```. 

```haskell
transition game s r = case (stateValue s, stateData s, r) of
```

- In the state machine we have the state S, which is a combination of datum and value; and you can access those two with state value and state data. The state value as is now the value in the UTxO that we're consuming state data S is the datum and R is the redeemer.

```haskell
lovelaces v == gStake game
```


- Then we checked that the value is actually the stake of the game.

**The first situation is:**

```haskell
  (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
```

- First, this must be signed by the second player.
- Second, was the validity interval. This needs to happen after the deadline.
- The third component  is the new state of the resulting UTxO which again is given by datum and value. Here we are specifying with this transition that the second player makes a move. The new datum will be``` bs $ Just c```. The new value will be twice this stake of the game because it's now both the first player and the second player stake. We leave the NFT out of here, even though it should be present in the UTxO.And that is again, because

**The second situation is:**

```haskell
 (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)
                                                     , State Finished mempty
                                                     )
```

The second case was the case where the second player has moved, which is reflected here in the Just. The first player realizes he has won and now wants to reveal his nonce in order to prove that he has won.

- First, this must be signed by the first player.
- Second, was the validity interval. This needs to happen after the deadline.
- Finally, we transition to this finished state.

**The third situation is:**

```haskell
   (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)
                                                     , State Finished mempty
```

The third case was the case where the second player does not move in time before the deadline. The first player wants to reclaim the stake.

- First, this must be signed by the first player.
- Second, this condition specifies after the deadline has passed. 
- Finally, we transition to this finished state.

**The fourth situation is:**

```haskell
 (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)
                                                     , State Finished mempty
```

Now the final case is the one where the second player has moved and the first player then does not reveal. This is because the first player has realized he has lost or because he left the game.

- First, this must be signed by the second player.
- Second, this condition specifies after the deadline has passed. 
- Finally, we transition to this finished state

```haskell
{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False
```

We then need to specify the final state. We set final finished ito true and everything else is false.


```haskell
{-# INLINABLE check #-}
check :: BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =
    sha2_256 (nonce `appendByteString` if c == Zero then bsZero' else bsOne') == bs
check _       _      _                       _              _ = True
```

Then we left out the nonce check in the transition function because we could express that as a constraint, so we need to declare that here.

```haskell
{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> BuiltinByteString -> BuiltinByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'
    , smThreadToken = Just $ gToken game
    }
```

Now we can define our state machine. We just provide the four fields that we just defined:

- Transition game state
- Final game state
- Additional check 
- Thread Token 

```haskell
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'
```

- Our old  ```mkGameValidator``` can now be replaced by using the state machine and using the make validator ```gameStateMachine```.

```haskell
type Gaming = StateMachine GameDatum GameRedeemer
```

Our type gaming can just be state machine ```GameDatum```, ```GameRedeemer```.

```haskell
gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne
```

- Our two strings are provided with a second version of gameStateMachine, where we only have to specify the game and not these two strings.

```haskell
typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer
```

- Now ```mkGameValidator``` has been defined here using the state machine mechanism, instead of doing it explicitly. The rest stay the same.

```haskell
gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)
```

- ```gameClient``` is a ```StateMachineClient```, and is what we need to interact with a state machine from our wallet, from our contract monad. In order to make a ```StateMachineClient```, we can use the ```mkStateMachineClient``` function,which takes a state machine instance to give us our game client. Now, the client can be used to interact with the state machine from off-chain code.

```haskell
mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show
```

- State machine contracts have a specific constraint on the error type. We want to do what we did in the last lectures and always use Text as a map errortype. In order to make those two fit together, we define this ```mapError’``` function, which uses the map error we have discussed before. It then turns an SM contract error into a text by showing the SM contract error and then picking the resulting string into a text.

```haskell
firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapError' getThreadToken
```

- We need to get the thread token. In order to do that, we must identify a UTxO in our wallet that can be used for the minting of the NFT to make that a true NFT. Then we have to apply the ```mapError’``` prime in order to convert it to text error messages. 

```haskell
void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
```

- ```runInitialise``` given the client, a datum and the value. First, it will mint the NFT corresponding to this thread token.
- Then it will create a UTxO at the state machine address to start the state machine. Put the NFT in that UTxO to uniquely identify it, and the datum and value of that UTxO are given by these arguments here. Therefore we put it in the initial state we want whether the first player commits using this hash. The second player has not moved yet, and the first player puts down his stake. We again, have to use the ```mapError’``` in order to adjust the error messages. 

```haskell
tell $ Last $ Just tt
```

- Here we need to communicate the thread token because the second player wants to find the game. Therefore, the second player must define this game. Part of that game is this thread token. The second player otherwise would have no way of knowing what the thread token is.

```haskell
m <- mapError' $ getOnChainState client
    case m of
        Nothing     -> throwError "game output not found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of
```

- We then use ```getOnchainState```. 


**getonChainState**

```haskell
getOnChainState :: (AsSMContractError e, FromData state, ToData state) => StateMachineClient state i -> Contract w schema e (Maybe (OnChainState state i, Map TxOutRef ChainIndexTxOut))

Get the current on-chain state of the state machine instance. Return Nothing if there is no state on chain. Throws an SMContractError if the number of outputs at the machine address is greater than one.
```

**onChainState**

```haskell
data OnChainState s i

Typed representation of the on-chain state of a state machine instance

Constructors
OnChainState
 
ocsTxOut :: TypedScriptTxOut (StateMachine s i)

Typed transaction output

ocsTxOutRef :: TypedScriptTxOutRef (StateMachine s i)

Typed UTXO

ocsTx :: ChainIndexTx

Transaction that produced the output
```

**TypedScriptTxOut**

```haskell
data TypedScriptTxOut a

A TxOut tagged by a phantom type: and the connection type of the output.
Constructors

(FromData (DatumType a), ToData (DatumType a)) => TypedScriptTxOut
 
tyTxOutTxOut :: TxOut
tyTxOutData :: DatumType a 
```

- So by using ```getOnchainState``` client, we have this ```m``` which could be nothing if no output is found as before. If it does, we throw an error and now we get this on-chain state.
- We are not interested in the reference only in the ```o``` itself. Then we use this ```tyTxOutData``` to directly access the datum.

```haskell
GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp
                logInfo @String "first player revealed and won"

            _ -> logInfo @String "second player played and won"
```

- Now we immediately have the datum.
- As before we have the two cases that the second player has or has not moved. 
- The important function here is ```runStep```.

**runStep**

```haskell
runStep

:: forall w e state schema input. (AsSMContractError e, FromData state, ToData state, ToData input)
 
-> StateMachineClient state input

The state machine

-> input

The input to apply to the state machine

-> Contract w schema e (TransitionResult state input)
 
Run one step of a state machine, returning the new state.
```

- runStep creates a transaction and submits it, that will transition the state machine. 


- It is the same for the second player, it's just as short.

Running this file along with TestStateMachine.hs should yield exactly the same results as the previous section.



## Homework 


The objective of the homework this week is to modify StateMachine.hs, and instead of a binary type game replace it with rock paper scissors (so now 3 options)

Lar’s had given the initial code:

```haskell
data GameChoice = Rock | Paper | Scissors
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock     == Rock     = True
    Paper    == Paper    = True
    Scissors == Scissors = True
    _        == _        = False

PlutusTx.unstableMakeIsData ''GameChoice

{-# INLINABLE beats #-}
beats :: GameChoice -> GameChoice -> Bool
beats Rock     Scissors = True
beats Paper    Rock     = True
beats Scissors Paper    = True
beats _        _        = False

data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False
```

- The big change here is now we have 3 game choices to handle the three options of rock paper and scissors.
- We specify the logic of power for which player will win using each combo of outcomes

The biggest change in the core logic of the state machine transition, is the second case. This is because we need to now handle both winning and also a draw. If it is a draw, the money needs to be sent back to the players.

```haskell
(v, GameDatum _ (Just c), Reveal _ c')
        | (lovelaces v == (2 * gStake game)) &&
          (c' `beats` c)                         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                           Constraints.mustValidateIn (to $ gRevealDeadline game)   
                                                         , State Finished mempty
                                                         )

        | (lovelaces v == (2 * gStake game)) &&
          (c' == c)                              -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                           Constraints.mustValidateIn (to $ gRevealDeadline game)       <>
                                                           Constraints.mustPayToPubKey (gSecond game)
                                                                                       (lovelaceValueOf $ gStake game)
                                                         , State Finished mempty
                                                         )
```

Alter the next functions to account for 3 game options now:

```haskell
check :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsRock' bsPaper' bsScissors' (GameDatum bs (Just _)) (Reveal nonce c) _ =
    sha2_256 (nonce `appendByteString` toBS c) == bs
  where
    toBS :: GameChoice -> BuiltinByteString
    toBS Rock     = bsRock'
    toBS Paper    = bsPaper'
    toBS Scissors = bsScissors'
check _ _ _ _ _ _ = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsRock' bsPaper' bsScissors' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsRock' bsPaper' bsScissors'
    , smThreadToken = Just $ gToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsRock' bsPaper' bsScissors' = mkValidator $ gameStateMachine game bsRock' bsPaper' bsScissors'

type Gaming = StateMachine GameDatum GameRedeemer

bsRock, bsPaper, bsScissors :: BuiltinByteString
bsRock     = "R"
bsPaper    = "P"
bsScissors = "S"

gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsRock bsPaper bsScissors

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode bsPaper
        `PlutusTx.applyCode` PlutusTx.liftCode bsScissors)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer
```
Lastly, we alter the client to handle the 3 options in firstGame:

```haskell
client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        y      = case c of
                    Rock     -> bsRock
                    Paper    -> bsPaper
                    Scissors -> bsScissors
        bs     = sha2_256 $ fpNonce fp `appendByteString` y
```

**The final code should look like:**

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.RockPaperScissors
     ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Rock | Paper | Scissors
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock     == Rock     = True
    Paper    == Paper    = True
    Scissors == Scissors = True
    _        == _        = False

PlutusTx.unstableMakeIsData ''GameChoice

{-# INLINABLE beats #-}
beats :: GameChoice -> GameChoice -> Bool
beats Rock     Scissors = True
beats Paper    Rock     = True
beats Scissors Paper    = True
beats _        _        = False

data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString GameChoice | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game             -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                           Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                         , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                         )
    (v, GameDatum _ (Just c), Reveal _ c')
        | (lovelaces v == (2 * gStake game)) &&
          (c' `beats` c)                         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                           Constraints.mustValidateIn (to $ gRevealDeadline game)   
                                                         , State Finished mempty
                                                         )

        | (lovelaces v == (2 * gStake game)) &&
          (c' == c)                              -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                           Constraints.mustValidateIn (to $ gRevealDeadline game)       <>
                                                           Constraints.mustPayToPubKey (gSecond game)
                                                                                       (lovelaceValueOf $ gStake game)
                                                         , State Finished mempty
                                                         )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game             -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                           Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)   
                                                         , State Finished mempty
                                                         )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)       -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                           Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)   
                                                         , State Finished mempty
                                                         )
    _                                            -> Nothing

{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE check #-}
check :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsRock' bsPaper' bsScissors' (GameDatum bs (Just _)) (Reveal nonce c) _ =
    sha2_256 (nonce `appendByteString` toBS c) == bs
  where
    toBS :: GameChoice -> BuiltinByteString
    toBS Rock     = bsRock'
    toBS Paper    = bsPaper'
    toBS Scissors = bsScissors'
check _ _ _ _ _ _ = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsRock' bsPaper' bsScissors' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsRock' bsPaper' bsScissors'
    , smThreadToken = Just $ gToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsRock' bsPaper' bsScissors' = mkValidator $ gameStateMachine game bsRock' bsPaper' bsScissors'

type Gaming = StateMachine GameDatum GameRedeemer

bsRock, bsPaper, bsScissors :: BuiltinByteString
bsRock     = "R"
bsPaper    = "P"
bsScissors = "S"

gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine' game = gameStateMachine game bsRock bsPaper bsScissors

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode bsPaper
        `PlutusTx.applyCode` PlutusTx.liftCode bsScissors)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapError' getThreadToken
    let game   = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = tt
            }
        client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        y      = case c of
                    Rock     -> bsRock
                    Paper    -> bsPaper
                    Scissors -> bsScissors
        bs     = sha2_256 $ fpNonce fp `appendByteString` y
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " ++ show (fpChoice fp)
    tell $ Last $ Just tt

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "game output not found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c `beats` c' || c' == c -> do
                logInfo @String "second player played and lost or drew"
                void $ mapError' $ runStep client $ Reveal (fpNonce fp) c
                logInfo @String "first player revealed and won or drew"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game   = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = spToken sp
            }
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play $ spChoice sp
                logInfo @String $ "made second move: " ++ show (spChoice sp)

                waitUntilTimeHasPassed $ spRevealDeadline sp

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won or drew"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame
```

**Then we can create a test file to test the output:**

```haskell

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Week07.TestRockPaperScissors
    ( test
    , test'
    , GameChoice (..)
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Week07.RockPaperScissors

test :: IO ()
test = do
    test' Rock Rock
    test' Rock Paper
    test' Rock Scissors
    test' Paper Rock
    test' Paper Paper
    test' Paper Scissors
    test' Scissors Rock
    test' Scissors Paper
    test' Scissors Scissors

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO $ myTrace c1 c2

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    let w1 = knownWallet 1
    let w2 = knownWallet 2

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let pkh1      = mockWalletPaymentPubKeyHash w1
        pkh2      = mockWalletPaymentPubKeyHash w2
        stake     = 5_000_000
        deadline1 = slotToEndPOSIXTime def 5
        deadline2 = slotToEndPOSIXTime def 10

        fp = FirstParams
                { fpSecond         = pkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"
                , fpChoice         = c1
                }

    callEndpoint @"first" h1 fp

    tt <- getTT h1

    let sp = SecondParams
                { spFirst          = pkh1
                , spStake          = stake
                , spPlayDeadline   = deadline1
                , spRevealDeadline = deadline2
                , spChoice         = c2
                , spToken          = tt
                }

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp

    void $ Emulator.waitNSlots 10
  where
    getTT :: ContractHandle (Last ThreadToken) GameSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt
```
