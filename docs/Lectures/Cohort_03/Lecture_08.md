Plutus Pioneer Program - Cohort 3 
March 9th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture8](https://youtu.be/mqHifIPefus)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1Z41CULV7lEXOZEOQDcjEN2ExWQ0fqsTydupI7jybC1U/edit?usp=sharing)

# Lecture 8: Property Based Testing

## Table of Contents

- [Lecture 8: Property Based Testing](#lecture-8-property-based-testing)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 8](#preparation-for-lecture-8)
  - [Introduction](#introduction)
  - [State Machine Example: A Token Sale](#state-machine-example-a-token-sale)
  - [Automatic Testing using Emulator Traces](#automatic-testing-using-emulator-traces)
  - [Test Coverage](#test-coverage)
  - [Optics](#optics)
  - [Property Based Testing with QuickCheck](#property-based-testing-with-quickcheck)
  - [Property Based Testing with Plutus Contracts](#property-based-testing-with-plutus-contracts)
  - [Homework](#homework)

## Preparation for Lecture 8

Before we can get started in lecture 8, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 8 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week08 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week08$ cat cabal.project
```

Grab the plutus-apps tag inside the cabal.project file:

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:c9c1e917edbfa3b972c92108d7b94d5430e07a28
```

Head back to the plutus-apps directory and update it to the current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout c9c1e917edbfa3b972c92108d7b94d5430e07a28
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week08 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week08]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week08]$ cabal build
```

We need to run this special repl command this week, because the directory structure of this week folder is split up:

```
[nix-shell:~/plutus-pioneer-program/code/week08]$ cabal repl plutus-pioneer-program-week08:test:plutus-pioneer-program-week08-tests
```

If successful,  you should now see in the terminal:

```
Output:
Ok, five modules loaded.

Prelude Main> 
```


We can now begin with the lecture.


## Introduction


In the last lecture we were introduced to state machines, and explored how they are very useful because they allow us to write much less code to express the logic of a smart contract.

We will look at another example since the concept is very important. This example will be of a token sale using the state machine implementation. Afterwards, we will look at testing; looking at optics, property based testing using QuickCheck, and finally property based testing using Plutus Contracts.


## State Machine Example: A Token Sale


This example is a contract that allows a person to sell tokens. The idea is that if we own the tokens, we can then sell some of them in this contract. We can then set a price for them, and then other people can buy them giving us the money they paid for it.

In the beginning, the seller starts with an NFT.


![ts1](https://user-images.githubusercontent.com/59018247/159122582-bd759429-23b3-46d4-9ef9-fefc519c33d3.png)



- it can be an arbitrary NFT 
- it will be used as before to identify the correct UTxO that contains the contract state.

The first step is for the seller to lock his NFT at the script address of the smart contract we are about to write, so we call that TS here for token sale. And as datum, we will use a simple integer, which indicates the price of the token and this starts off as zero.



![ts2](https://user-images.githubusercontent.com/59018247/159122602-34c3a994-7c49-4e4c-aefe-27b4f5f2b93a.png)




There will be several operations that the seller can do. 

- One of those is setting the price to a different value. 

In order to do that, the seller needs to submit a transaction that has the current UTxO as input, and the updated UTxO as output. In this example, the seller set the price to six lovelace per token.




![ts3](https://user-images.githubusercontent.com/59018247/159122632-e55425d0-3f01-47cc-8f8a-a8eec2f82eca.png)




Another thing the seller can do is lock some tokens in the contract. In order to do that they have to create another transaction which has as input, the UTxO of the contract and a UTxO containing some tokens; as output, the updated UTxO at the contract address which now contains the provided tokens. So in this example, the seller provides five tokens to the contract.



![ts4](https://user-images.githubusercontent.com/59018247/159122646-89843ca6-09b7-4e75-b946-03eadd62b23e.png)




In order to buy tokens, there needs to be a transaction created by the buyer that as input, of course, again has this UTxO sitting at the TS script address and the buying price in lovelace. If a person wants to buy two tokens, they would create a transaction that has as input, the price 12 lovelace, and the UTxO at the TS script address.  Then two outputs, one the updated contract state, where now the tokens are taken out and the price has been added. One output also going to the buyer, with the tokens he just bought.



![ts5](https://user-images.githubusercontent.com/59018247/159122659-81fc25b2-185b-4247-bee0-1226ee03cfe6.png)




Finally, there must be a way for the seller to retrieve tokens and ADA. In this example if, after the sale, the seller wants to retrieve all the Ada and one token, they would create a transaction that, again, has the script UTxO as input, and, as output, the updated script UTxO with the reduced balances, and one to themselves with the retrieved funds.
The diagram just shows one scenario, but these operations can be performed in any order - tokens can be added, the price can be changed, tokens can be bought, and so on, in an arbitrary order.


We will now look at Week08.TokenSale as an example:

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

{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Week08.TokenSale
    ( TokenSale (..)
    , TSRedeemer (..)
    , tsCovIdx
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    , useEndpoints'
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Code                (getCovIdx)
import           PlutusTx.Coverage            (CoverageIndex)
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

data TokenSale = TokenSale
    { tsSeller :: !PaymentPubKeyHash
    , tsToken  :: !AssetClass
    , tsTT     :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p v
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    _                                       -> Nothing

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsTT ts) (transition ts) (const False)

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine Integer TSRedeemer

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

tsCovIdx :: CoverageIndex
tsCovIdx = getCovIdx $$(PlutusTx.compile [|| mkTSValidator ||])

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

startTS :: AssetClass -> Contract (Last TokenSale) s Text ()
startTS token = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapErrorSM getThreadToken
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts

setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ startTS . AssetClass

useEndpoints' :: ( HasEndpoint "set price" Integer s
                 , HasEndpoint "add tokens" Integer s
                 , HasEndpoint "buy tokens" Integer s
                 , HasEndpoint "withdraw" (Integer, Integer) s
                 )
              => TokenSale
              -> Contract () s Text ()
useEndpoints' ts = forever
                $ handleError logError
                $ awaitPromise
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw'
  where
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ Prelude.uncurry $ withdraw ts

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints = useEndpoints'
```

And the first definition here is called ```TokenSale```:

```haskell
data TokenSale = TokenSale
    { tsSeller :: !PaymentPubKeyHash
    , tsToken  :: !AssetClass
    , tsTT     :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)
```

It parameterizes this ```TokenSale```, so it is a record type with three fields.

- First, the PaymentPubKeyHash of the seller ```tsSeller```
- Then an asset class called ```tsToken```, that is the token being sold.
- Finally, ```tsTT``` of type thread token used by the state machine to carry the state

```haskell
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    deriving (Show, Prelude.Eq)
```
For ```TSRedeemer```, we provide exactly the operations we saw in the diagram.

- ```SetPrice``` to a new value, this is the token price in lovelace.
- ```AddTokens```, where the argument gives the amount of tokens to add.
- ```BuyTokens```, where the argument gives the amount of tokens to buy.
- Finally, ```Withdraw```. In this case, the first one is how many tokens to withdraw. The second one is how many lovelaces to withdraw.

```haskell
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```
- ```lovelaces``` is that, given the value  it extracts the amount of lovelaces.

```haskell
{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p v
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    _                                       -> Nothing

```haskell
{-# INLINABLE transition #-}
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
```

Now we look at the ```transition``` function of the state machine. 

- The first argument ```TokenSale``` is the parameter 
- We get the state and in this case for datum we will use integer
- The price of the token and the redeemer.
- Then we must return a maybe constrained state, so we return nothing if the corresponding transition is illegal. Then we must provide the constraints and the new state which contains the datum and the value.
Similar to last time we split this given state into the value and the datum.


```haskell
(v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p v
                                                    )
```

The first case is ```SetPrice```.

- The seller wants to set the price to the new value ```p```. We only allow that when the price is non-negative.
- In that case, the transition is allowed if the transaction has been signed by the seller. 
- The new datum should be ```p```, the value given here in the redeemer. This value should not change.
- The ```v``` is the old value.


```haskell
(v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) n
                                                    )
```

The second case is ```AddTokens```.

- The amount of tokens to add is positive
- In this case, there is no constraint
- The new state, where the price doesn't change but the value changes by this amount of tokens

```haskell
 (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
```

The third case is ```BuyTokens```.

- There is no constraint, any person can buy tokens.
- In the new state, the price again does not change. The value changes, so we have the original value.
- We subtract the amount of tokens that are bought, which again is a positive number. You can only buy a positive number of tokens.
- Price is just the lovelace value of the amount of tokens multiplied by the current price which is this ```p```.


```haskell
(v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )

```
The fourth case we have the ```Withdraw``` operation.

- Both amounts are greater or equal zero.
- In this case, it  does have to be signed by the seller; because only the seller wants to be allowed to withdraw from the contract
- In the new state, again the price doesn't change
- All other transitions are illegal, so in all other cases we return nothing in the transition function.

```haskell
{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsTT ts) (transition ts) (const False)
```

Now that we have the transition function, we can easily define our state machine. In this case we can use a smart constructor called make state machine.

The idea is that once this token sale state machine has been started it will run forever, there is no operation to stop it again. This gives us our state machine.

```haskell
{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine Integer TSRedeemer

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)
```

- We can turn the state machine into a validator function like we saw last time.
- Then we get our typed validator with the usual template Haskell.
- From that we get the validator and the address.
- We can also as we saw last time easily define the state machine client, which is used to interact with the state machine from off-chain code, from the wallet.

```haskell
tsCovIdx :: CoverageIndex
tsCovIdx = getCovIdx $$(PlutusTx.compile [|| mkTSValidator ||])
```

The next piece here is ```tsCovIdx```, which stands for coverage index. As the name suggests, it is for getting coverage information for tests.

```haskell
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show
```

Then we need one helper function that's also similar to last time, because we want to use these operations from the state machine library. 

- They use their own custom error type, ```SMContractError```.

```haskell
startTS :: AssetClass -> Contract (Last TokenSale) s Text ()
startTS token = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapErrorSM getThreadToken
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
```

Now, let's look at the off-chain code. The first function we have is called ```startTS```, start token sale; and it takes one parameter.

- It does not have a result, but it uses writer functionality with ```Last TokenSale```.

First, we ask for our ```ownPaymentPubKeyHash```.

- Then we use the get thread token function ```getThreadToken```.
- We have to use this ```mapErrorSm``` function to convert the error to text.

So with these two pieces of information, we can define a value of type token sale.

```haskell
setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l
```

Now we have simple functions corresponding to the various actions: 

- ```setPrice```
- ```addTokens```
- ```buyTokens```
- ```withdraw```

All we have to do is ```runStep``` and provide the client and the action we want to take. In every case, we have to use this ```mapErrorSM``` to convert the error messages accordingly.

```haskell
type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
```
Now we can bundle everything up, so we define two schemas:

- One to start the token sale  
- One to use it

The start schema only has one endpoint that we called start. For use, we again have our four actions.

- set price takes the new price as an argument.
- add tokens takes the amount of tokens to add as an argument.
- buy tokens takes the amount of tokens to buy as an argument.
- withdraw takes a pair of integers; the amount of tokens we want to withdraw and the amount of lovelace we want to withdraw.

```haskell
startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ startTS . AssetClass

useEndpoints' :: ( HasEndpoint "set price" Integer s
                 , HasEndpoint "add tokens" Integer s
                 , HasEndpoint "buy tokens" Integer s
                 , HasEndpoint "withdraw" (Integer, Integer) s
                 )
              => TokenSale
              -> Contract () s Text ()
useEndpoints' ts = forever
                $ handleError logError
                $ awaitPromise
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw'
  where
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ Prelude.uncurry $ withdraw ts
```


Then we can define startEndpoint and useEndpoints that now use these schemas.

```haskell
runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace
```

In order to try it out, let's first use the emulator as we did before.So we define a run my trace function that again uses run emulator trace IO' with a custom emulator configuration and my trace.

```haskell
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000
```

Let's first look at the custom emulator configuration where we specify an initial distribution. This does not only give ADA to the wallets, but also another token.The idea is every wallet gets 1000 ADA and 1000 tokens.

```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet w1 startEndpoint
    callEndpoint @"start" h (currency, name)
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet w1 $ useEndpoints ts
            h2 <- activateContractWallet w2 $ useEndpoints ts
            h3 <- activateContractWallet w3 $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5

checkPredicateOptionsCoverage :: CheckOptions
                              -> String
                              -> CoverageRef
                              -> TracePredicate
                              -> EmulatorTrace ()
                              -> TestTree
checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action =
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep<>))
```

Now for the trace.

- First, we activate the start endpoint for wallet one. Wallet one will be the one running the token sale. As parameters, we give our currency and name that we defined.
- Then we just wait for five slots to give this enough time to start the state machine.
- We ask for the observable state,the token sale value that has been started.

Then there are two possibilities:

- ```m``` is nothing, in which case the token sale hasn't started yet or something went wrong so we log an error message.
- The other possibility, we do get our ```ts``` of type token sale.

Now we can start the use endpoints which are parameterized by ts value. We need to know ts in order to start the use endpoints.

We start them on all three wallets, one two three. Then we call various endpoints.

- The first, wallet one sets the price to 1 ADA.

We wait for five slots. 

- Now wallet one adds 100 tokens to the token sale UTxO.

We wait for another five slots.

- Now wallet two buys 20 tokens.And because the price at the moment is 1 ADA, that should cost 20 ADA.

We wait for another five slots.

- Wallet three buys five tokens, which should cost 5 ADA, because the price is still 1 ADA.

We wait again.

- Finally, wallet one withdraws 40 tokens and 10 ADA.

So originally there were 100 tokens; 25 tokens have been bought that leaves 75 tokens. 40 are then withdrawn, so 35 tokens should be left. Initially there was no ADA, but wallet two paid 20 ADA and wallet three paid 5 ADA, which makes 25 ADA. So if we withdraw 10 ADA, then 15 ADA should be left.

Trying this out in the repl:

```haskell
cabal repl plutus-pioneer-program-week08:test:plutus-pioneer-program-week08-tests

Output:
Ok, five modules loaded.

Prelude Main> : l test/Spec/Trace.hs

Output:
Ok, one module loaded.

Prelude Spec.Trace> runMyTrace

Output:

Slot 00000: TxnValidate 2125c8770581c6140c3c71276889f6353830744191de0184b6aa00b185004500
Slot 00000: SlotAdd Slot 1
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Contract instance started
```


The first endpoint call is to start. This creates three transactions; two of these are from the forge contract to create the NFT, and the third one is to set up our initial UTxO for the token sale.

```
Slot 00001: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Receive endpoint call: Object (fromList [("tag",String "start"),("value",Object (fromList [("unEndpointValue",Array [Object (fromList [("unCurrencySymbol",String "aa")]),Object (fromList [("unTokenName",String "A")])])]))])
Slot 00001: W1: TxSubmit: cccba8b2abc3e82a735735c2346aa3fcac58152f17854b1745306e5b63a0b965
Slot 00001: TxnValidate cccba8b2abc3e82a735735c2346aa3fcac58152f17854b1745306e5b63a0b965
Slot 00001: SlotAdd Slot 2
Slot 00002: W1: TxSubmit: e23e19192aea3304a989ab98f05e70bc01fe43f3ea940da78a92ab7cebec9bbb
Slot 00002: TxnValidate e23e19192aea3304a989ab98f05e70bc01fe43f3ea940da78a92ab7cebec9bbb
Slot 00002: SlotAdd Slot 3
Slot 00003: W1: TxSubmit: 4cae1c5115eb4128243ce029dcd4d6c23d6497d3ab5e71a79f4dc34e9b8cd763
Slot 00003: TxnValidate 4cae1c5115eb4128243ce029dcd4d6c23d6497d3ab5e71a79f4dc34e9b8cd763
Slot 00003: SlotAdd Slot 4
Slot 00004: *** CONTRACT LOG: "started token sale TokenSale {tsSeller = 21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9, tsToken = (aa,\"A\"), tsNFT = (65b4199f7d025bfb3b065b0fb88a77d694ffd849ff740b1a4cc453bfaab30f55,\"NFT\")}"
Slot 00004: SlotAdd Slot 5
Slot 00005: SlotAdd Slot 6
```

We successfully read the TokenSale value from the observable state, and start the three contract instances for the use contract.

```
Slot 00006: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
  Sending contract state to Thread 0
Slot 00006: SlotAdd Slot 7
Slot 00007: *** USER LOG: started token sale TokenSale {tsSeller = 21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9, tsToken = (aa,"A"), tsNFT = (65b4199f7d025bfb3b065b0fb88a77d694ffd849ff740b1a4cc453bfaab30f55,"NFT")}
Slot 00007: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 1}:
  Contract instance started
Slot 00007: 00000000-0000-4000-8000-000000000002 {Contract instance for wallet 2}:
  Contract instance started
Slot 00007: 00000000-0000-4000-8000-000000000003 {Contract instance for wallet 3}:
  Contract instance started
```

Then we set the price.

```
Slot 00007: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 1}:
  Receive endpoint call: Object (fromList [("tag",String "set price"),("value",Object (fromList [("unEndpointValue",Number 1000000.0)]))])
Slot 00007: W1: TxSubmit: 2de6dd820e6939b4b1f9e162c0e2cc878cc38ea1231a9be610315da4eda06714
Slot 00007: TxnValidate 2de6dd820e6939b4b1f9e162c0e2cc878cc38ea1231a9be610315da4eda06714
Slot 00007: SlotAdd Slot 8
Slot 00008: SlotAdd Slot 9
Slot 00009: SlotAdd Slot 10
Slot 00010: SlotAdd Slot 11
Slot 00011: SlotAdd Slot 12
```

Then add some tokens.

```
Slot 00012: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 1}:
  Receive endpoint call: Object (fromList [("tag",String "add tokens"),("value",Object (fromList [("unEndpointValue",Number 100.0)]))])
Slot 00012: W1: TxSubmit: 42f1bebe285d1ea23bd90683d110866bb438eede8ef62eaf5e9e3d65eec18e90
Slot 00012: TxnValidate 42f1bebe285d1ea23bd90683d110866bb438eede8ef62eaf5e9e3d65eec18e90
Slot 00012: SlotAdd Slot 13
Slot 00013: SlotAdd Slot 14
Slot 00014: SlotAdd Slot 15
Slot 00015: SlotAdd Slot 16
Slot 00016: SlotAdd Slot 17
```

Then the two buys initiated by Wallets 2 and 3.

```
Slot 00017: 00000000-0000-4000-8000-000000000002 {Contract instance for wallet 2}:
  Receive endpoint call: Object (fromList [("tag",String "buy tokens"),("value",Object (fromList [("unEndpointValue",Number 20.0)]))])
Slot 00017: W2: TxSubmit: 30d28ca855a14accbb11deee682b174adffb548922e1d4257242880f28328f8e
Slot 00017: TxnValidate 30d28ca855a14accbb11deee682b174adffb548922e1d4257242880f28328f8e
Slot 00017: SlotAdd Slot 18
Slot 00018: SlotAdd Slot 19
Slot 00019: SlotAdd Slot 20
Slot 00020: SlotAdd Slot 21
Slot 00021: SlotAdd Slot 22
Slot 00022: 00000000-0000-4000-8000-000000000003 {Contract instance for wallet 3}:
  Receive endpoint call: Object (fromList [("tag",String "buy tokens"),("value",Object (fromList [("unEndpointValue",Number 5.0)]))])
Slot 00022: W3: TxSubmit: 708b0c4117ad3b38b69254a714e4695c574af404c3fff0eda859b571218b003c
Slot 00022: TxnValidate 708b0c4117ad3b38b69254a714e4695c574af404c3fff0eda859b571218b003c
Slot 00022: SlotAdd Slot 23
Slot 00023: SlotAdd Slot 24
Slot 00024: SlotAdd Slot 25
Slot 00025: SlotAdd Slot 26
Slot 00026: SlotAdd Slot 27
```

And finally, the withdrawal by Wallet 1.

```
Slot 00027: 00000000-0000-4000-8000-000000000001 {Contract instance for wallet 1}:
  Receive endpoint call: Object (fromList [("tag",String "withdraw"),("value",Object (fromList [("unEndpointValue",Array [Number 40.0,Number 1.0e7])]))])
Slot 00027: W1: TxSubmit: a42a06cc3e3b1653ec4aba5ab8304484d778adcbddac2ceb9f639f7e4bd1dfd2
Slot 00027: TxnValidate a42a06cc3e3b1653ec4aba5ab8304484d778adcbddac2ceb9f639f7e4bd1dfd2
Slot 00027: SlotAdd Slot 28
Slot 00028: SlotAdd Slot 29
Slot 00029: SlotAdd Slot 30
Slot 00030: SlotAdd Slot 31
Slot 00031: SlotAdd Slot 32
Slot 00032: SlotAdd Slot 33
```

All wallets initially owned 1000 tokens and 1000 Ada. Wallet 1 added 100 tokens to the contract, but then in the last step retrieved 40 tokens and 10 Ada, and so we see its final balance as 940 tokens and 1010 Ada minus transaction fees.

```
Final balances
Wallet 1: 
    {aa, "A"}: 940
    {, ""}: 1009942570
```

Wallet 2 bought 20 tokens and paid 20 ADA for them, plus some transaction fees.

```
Wallet 2: 
    {aa, "A"}: 1020
    {, ""}: 979985260
```

Wallet 3 bought 5 tokens for 5 Ada.

```
Wallet 3: 
    {aa, "A"}: 1005
    {, ""}: 994985211
```

Finally, the script still contains the NFT, which will forever stay there, plus 35 tokens and 15 Ada. There were, at one point, 75 tokens and 25 Ada, before Wallet 1 made a withdrawal.

```
Script fb3eca878d177b6d9264c7c36845fb1e28935553812ed2b56e39c9c4564b85ad: 
    {65b4199f7d025bfb3b065b0fb88a77d694ffd849ff740b1a4cc453bfaab30f55, "NFT"}: 1
    {aa, "A"}: 35
    {, ""}: 15000000
```

## Automatic Testing using Emulator Traces

There are various testing frameworks in Haskell, test harnesses that can organize, label, and group your tests. Plutus uses the Tasty test framework.

You can find tasty on hackage: 

[https://hackage.haskell.org/package/tasty](https://hackage.haskell.org/package/tasty) 

```haskell
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
```

As an example, you have a main program that uses default main and some tests. These tests are of type test tree; as the name suggests, a test tree is a tree of tests. You can group tests and have subgroups and sub subgroups.

There's special support for tests in Plutus. This is provided in  module Plutus.Contract.Test [https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test.html](https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract-Test.html) in the plutus-contract package.

There are various types of tests that are supported. But today we will only look at two of those.
- One that works with emulator traces.
- One that is much more sophisticated and uses property-based testing

```haskell
checkPredicate
:: String
Descriptive name of the test
-> TracePredicate
The predicate to check
-> EmulatorTrace ()
 
-> TestTree
 

Check if the emulator trace meets the condition
```

Let's start with the emulator trace based tests. In this module, there's the chapter on checking predicates.
- There is a check predicate function; it takes a descriptive name of the test.
- Then a trace predicate. 
- Then an emulator trace like the one we just used to manually test our contract.

The result is a test tree, which as we just saw, is the type of tests that Tasty uses. By using this check predicate, we can produce something that the tasty framework can understand.

```haskell
checkPredicateOptions
:: CheckOptions
Options to use
-> String
Descriptive name of the test
-> TracePredicate
The predicate to check
-> EmulatorTrace ()
 
-> TestTree
 

A version of checkPredicate with configurable CheckOptions
```

There's also a variation, check predicate options, where we can set some options. Furthermore, there is one additional argument of type check options.

```haskell
data CheckOptions
Options for running the
defaultCheckOptions :: CheckOptions
minLogLevel :: Lens' CheckOptions LogLevel
emulatorConfig :: Lens' CheckOptions EmulatorConfig
changeInitialWalletValue :: Wallet -> (Value -> Value) -> CheckOptions -> CheckOptions
Modify the value assigned to the given wallet in the initial distribution.
```

Going back to the check predicate function and its cousins. The one ingredient we haven't looked at yet is ```TracePredicate```:

```haskell
type TracePredicate = FoldM (Eff '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void)]) EmulatorEvent Bool
not :: TracePredicate -> TracePredicate
(.&&.) :: TracePredicate -> TracePredicate -> TracePredicate infixl 3
(.||.) :: TracePredicate -> TracePredicate -> TracePredicate infixl 2
```

The ```TracePredicate``` is a predicate on a trace as the name suggests, so a condition on a trace; that is the actual test. Given a trace, it is a condition that will be checked by the test.

There are all sorts of things you can check that a given contract has run to completion; or not run to completion the endpoint is available that an error happened. 

But we, as an example, will only use one namely ```walletFundsChange```:

```haskell
walletFundsChange :: Wallet -> Value -> TracePredicate
Check that the funds in the wallet have changed by the given amount, excluding fees.
```

This will check that for a given wallet after the trace is completed, the wallet funds will have changed by this given value. If you look at the comment, then in particular it says here excluding fees. So, often as we saw in the examples, the value change is not really predictable, because there are always these fees and the fee calculation is quite complicated. This predicate allows us to ignore the fees and just look at the value change without fees. However, if you insist, there is also a variation of this value walletFunds where the fee is not automatically taken care of.


```haskell
tests :: TestTree
tests = checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    myTrace
```

Going back to the module, we can now look at this tests function. 
- We are using the checkPredicateOptions variation, because we need to specify the options in order to communicate that we want to use the emulator config where we specify the initial distribution of funds.

```haskell
myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg
```

The way we define these options is we start with the ```defaultCheckOptions```. Then we set the ```emulatorConfig``` part to ```emCfg``` that we had defined before. 

- This weird operator ```.~ emCfg```, is part of optics, but it basically says to set the emulatorConfig part of the check options to this value; but we will look at optics briefly later.

Now the interesting part is the myPredicate. And if this is defined here:

```haskell
myPredicate :: TracePredicate
myPredicate =
    walletFundsChange w1 (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60) <> Plutus.negate (toValue minAdaTxOut)) .&&.
    walletFundsChange w2 (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)                                         .&&.
    walletFundsChange w3 (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
```

We use this logic to combine three predicates, using this ```walletFundsChange```. 

Now we can test this in the repl:

```haskell
Prelude Main> import Test.Tasty

Prelude Test.Tasty> :l test/Spec/Trace.hs

Prelude Test.Tasty Spec.Trace> defaultMain tests

token sale trace: OK (1.22s)

All 1 tests passed (1.22s)
*** Exception: ExitSuccess
```

This passes. Let's see what happens if it doesn't pass. We can change one of the values.

```haskell
( walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-50) )
```

```haskell
Prelude Test.Tasty Spec.Trace> :l Spec.Trace

[1 of 1] Compiling Spec.Trace       ( test/Spec/Trace.hs, /home/chris/git/ada/pioneer-fork/code/week08/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-pioneer-program-week08-0.1.0.0/t/plutus-pioneer-program-week08-tests/build/plutus-pioneer-program-week08-tests/plutus-pioneer-program-week08-tests-tmp/Spec/Trace.o )
Ok, one module loaded.
Prelude Test.Tasty Spec.Trace> defaultMain tests
token sale trace: FAIL (1.32s)
  Expected funds of W1 to change by
    Value (Map [(,Map [("",10000000)]),(aa,Map [("A",-50)])])
    (excluding 57430 lovelace in fees)
  but they changed by
    Value (Map [(,Map [("",10000000)]),(aa,Map [("A",-60)])])
  Test failed.
  Emulator log:

  [INFO] Slot 0: TxnValidate 2125c8770581c6140c3c71276889f6353830744191de0184b6aa00b185004500
  [INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
                   Contract instance started
  [INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Contract instance for wallet 1}:
                   Receive endpoint call: Object (fromList [("tag",String "start"),("value",Object (fromList [("unEndpointValue",Array [Object (fromList [("unCurrencySymbol",String "aa")]),Object (fromList [("unTokenName",String "A")])])]))])
  [INFO] Slot 1: W1: Balancing an unbalanced transaction:
  ...
  ...
  [INFO] Slot 27: W1: TxSubmit: a42a06cc3e3b1653ec4aba5ab8304484d778adcbddac2ceb9f639f7e4bd1dfd2
  [INFO] Slot 27: TxnValidate a42a06cc3e3b1653ec4aba5ab8304484d778adcbddac2ceb9f639f7e4bd1dfd2
    src/Plutus/Contract/Test.hs:245:
    token sale trace

1 out of 1 tests failed (1.32s)
*** Exception: ExitFailure 1
```
We see an error message, followed by the emulator log, which we didn't get when the tests passed.
This is probably the simplest way to write automated tests for Plutus contracts. You simply write one or more emulator traces, and then use checkPredicate in association with the appropriate test predicates, to check that the trace leads to the desired result. This lets us write more or less traditional unit tests.


## Test Coverage


```haskell
checkPredicateOptionsCoverage :: CheckOptions
                              -> String
                              -> CoverageRef
                              -> TracePredicate
                              -> EmulatorTrace ()
                              -> TestTree
checkPredicateOptionsCoverage options nm (CoverageRef ioref) predicate action =
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm) (\rep -> modifyIORef ioref (rep<>))
```

Looking at the code in the bottom of the Spec.Trace module, We implemented this ```checkPredicateOptionsCoverage``` function. We looked at the implementation of ```checkPredicateOptions``` and ```checkPredicateCoverage``` and then combined the two.

```CoverageRef``` is just a new type around an IO ref of coverage report.

- For those of you who are not very familiar with Haskell, IO ref is an IO reference and that allows you to do mutual references, mutable variables in IO.

```haskell
testCoverage :: IO ()
testCoverage = do
    cref <- newCoverageRef
    e <- try $ defaultMain $ checkPredicateOptionsCoverage
        myOptions
        "token sale trace"
        cref
        myPredicate
        myTrace
    case e of
        Left (c :: ExitCode) -> do
            putStrLn $ "Tasty exited with: " ++ show c
            report <- readCoverageRef cref
            writeCoverageReport "TokenSaleTrace" tsCovIdx report
        Right () -> putStrLn $ "unexpected tasty result"
```

Using that, we wrote this function called ```testCoverage```. 

- First we use this ```newCoverageRef``` to get such a ```cref```. 
- Then, we want to use my ```checkPredicateOptionsCoverage``` but of course, now I am in IO. So somehow we have to execute this in IO.
- We are using this ```defaultMain``` that we saw earlier to run our normal unit tests.
- ```defaultMain``` throws an exception at the end, even if the tests are successful. It always throws a so-called exit-code exception; either with exit-code 0 if the tests ran successfully or with exit-code non-zero if there was a failure. That is how the testing framework signals to cabal whether a test suite failed or succeeded by using these exit-codes. Nevertheless, this interferes  with the current stuff we want to do, so we are catching this exit exception.

Then we provide the earlier options we had, the same name. 

- We then have to catch this exception in the expected cases that there is an exception,because this default main is supposed to always throw this exit-code exception.

- Then we have this function ```writeCoverageReport``` that takes the name of the report and it takes this report using the coverage index

```haskell
Prelude Main> import Test.Tasty

Prelude Test.Tasty> :l test/Spec/Trace.hs

Prelude Test.Tasty Spec.Trace> testCoverage

token sale trace: OK (0.21s)

All 1 tests passed (0.21s)
Tasty exited with: ExitSuccess
```

![coveragepass](https://user-images.githubusercontent.com/59018247/159125412-1be993fc-ed56-4e08-b449-9f4641ba605d.png)


So let's look at what that looks like. If we call this test coverage function, then it will create an html file in the week 8 code folder. We can actually look at this file in a browser. Then we get this rendering of the token sale module with colors indicating the coverage. So in particular, we see that in our transition function, this has been covered. But in these guard conditions we see this condition was always true. 

The green means in all the tests this condition was always true which makes sense, because in our trace we only set the price to non-negative values. We also see that this catch-all case here was never hit. This means this branch of the transition function has not been covered by our code.



![coveragefail](https://user-images.githubusercontent.com/59018247/159125452-169a903a-9fd2-46fd-a06b-7843a5cd1cd7.png)




Let's change the trace briefly and see how that changes this coverage report. So if we just comment this last step here in the trace where we withdraw. 

```haskell
{-
            callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5
-}
```


If we reload and run this again, the test fails now which makes sense. Now the wallet value changes of wallet one are different, because the withdrawal is now missing. If we reload this coverage report here, then now we see the withdrawal transition is black and the code here is red. That means that this is no longer covered by our trace.

## Interlude: Optics


Before we get to the second way of testing Plutus contracts, we will take a brief look at optics and lenses.There are various competing optics libraries on Hackage, but the most prominent, and the most infamous one, and the one that the Plutus team decided to use is called Lens.

https://hackage.haskell.org/package/lens

Lens is authored by Edward Kmett, who is probably the most prolific contributor to Haskell libraries.



![optics](https://user-images.githubusercontent.com/59018247/159125566-c768fc45-7b41-4195-98a2-38185884b861.png)




This is a nice diagram with  some of the operations that  the lens library provides. Optics are about reaching deeply into hierarchical data types and inspecting parts that are deeply hidden in a data type and manipulate them.


```haskell 
{-# LANGUAGE TemplateHaskell #-}

module Week08.Lens where

import Control.Lens

newtype Company = Company {_staff :: [Person]} deriving Show


data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }

goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}

makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
```

Let's look at a very simple example that we provide in module Lens that shows the problem and how optics addressed that problem.

```haskell
newtype Company = Company {_staff :: [Person]} deriving Show
```
So let's look at the company data type. It's just a newtype wrapper around a list of persons with the accessor called staff. We provide an underscore in front of staff that has no semantic meaning, when dealing with lenses; it's just convention to call fields with a leading underscore.Then we have this company  type wrapper around persons.

```haskell
data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show
```

Then ```Person``` has the record type with two fields, name and address. Where name is just a string and address is of type address, again, following this convention to use underscores in front of names.

```haskell
newtype Address = Address {_city :: String} deriving Show
```

Finally, address is yet another newtype wrapper around a string with an accessor called ```_city```. 

```haskell
alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }
```

And just as an example, we define two people, Alejandro and Lars with name and address. Finally, we define a company where the staff consists of these two persons.

```haskell
goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}

makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
```

The task is to write a simple function, ```goTo```, that gets a String as argument along with a Company. The function should create a new company which it gets by changing all the cities of all the staff of the company with the given string.
If we apply that to iohk with a string argument of Athens, then we should get a Company with the same two Persons, but now both of those Persons have a city of Athens.
You don't need any advanced Haskell to achieve this, but it's a bit messy, even in this simple example. The function below uses record syntax to modify specific fields of records, while leaving the other fields the same.
The helper function movePerson updates the ```_address``` field of the Person ```p```, and the ```_city``` field of that Address, and the main part of the function maps the movePerson function over each member of ```_staff```.

We can  try this out in the repl:

```haskell
Prelude Main> :l src/Week08/Lens.hs

Prelude Week08.Lens> iohk 

Output:
Company {_staff = [Person {_name = "Alejandro", _address = Address {_city = "Zacateca"}},Person {_name = "Lars", _address = Address {_city = "Regensburg"}}]}

Prelude Test.Tasty Spec.Trace> goTo "Athens" iohk

Output:
Company {_staff = [Person {_name = "Alejandro", _address = Address {_city = "Athens"}},Person {_name = "Lars", _address = Address {_city = "Athens"}}]}
```

So, dealing with nested record types, even though it is quite simple conceptually, can be quite messy.
This is what optics try to make easier with the idea of providing first-class field accessors. In the end it's very similar to dealing with such data types in an imperative language such as C# or Java.
We saw in lecture four how monads can be viewed as a programmable semi-colon, where the semi-colon is the statement separator in many imperative languages. In a similar way, optics can be thought of as providing a programmable dot, where a dot is the accessor dot as in Python or Java.
You could implement lenses by hand, but the lens library provides some Template Haskell magic to do it automatically, so long as we follow the underscore convention mentioned above.

```haskell
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address
```

So we added these make lenses company, make lenses person, make lenses address. The names of the lenses will be the names of the original fields without the underscore. The fields will have the  underscore and the lenses won't.

There's a way to inspect what code template Haskell writes at compile time from the repl.

```haskell
Prelude Week08.Lens> :set -ddump-splices
```
For that, you can activate a  flag called ```--ddump-splices```. Then, reload the module. If nothing happens, you'll need to make a minor change to the code, perhaps by adding some whitespace, before reloading.

```haskell
Prelude Week08.Lens> :r

Output:

makeLenses ''Company
  ======>
    staff :: Iso' Company [Person]
    staff = (iso (\ (Company x_abBO) -> x_abBO)) Company
    {-# INLINE staff #-}
src/Week08/Lens.hs:36:1-19: Splicing declarations
    makeLenses ''Person
  ======>
    address :: Lens' Person Address
    address f_abEJ (Person x1_abEK x2_abEL)
      = (fmap (\ y1_abEM -> (Person x1_abEK) y1_abEM)) (f_abEJ x2_abEL)
    {-# INLINE address #-}
    name :: Lens' Person String
    name f_abEN (Person x1_abEO x2_abEP)
      = (fmap (\ y1_abEQ -> (Person y1_abEQ) x2_abEP)) (f_abEN x1_abEO)
    {-# INLINE name #-}
src/Week08/Lens.hs:37:1-20: Splicing declarations
    makeLenses ''Address
  ======>
    city :: Iso' Address String
    city = (iso (\ (Address x_abFw) -> x_abFw)) Address
    {-# INLINE city #-}
```

This now shows us what Template Haskell does.

We see that ```makeLenses``` for Company creates a function staff, which returns an Iso' - a type of optic - from Company to [Person].

For ```makeLenses Person``` we get an address function which returns a Lens' from Person to Address, and we also get a name lens from Person to String.

For ```makeLenses Address``` we get a city function which returns an Iso' from Address to String.

Iso and Lens are two different types of optics but the order of type arguments is always the same. You always have two type arguments, at least for these primed versions (there are more general optics which take four type parameters). The first argument is always the big data type and the second parameter is the part you are zooming into. The name optics relates to the mental image of zooming into a datatype.

Let's try them out in the repl.

```haskell
Prelude Week08.Lens> lars

Output:
Person {_name = "Lars", _address = Address {_city = "Regensburg"}}

Prelude Week08.Lens> import Control.Lens

Prelude Control.Lens Week08.Lens> lars ^. Name

Output:
"Lars"

Prelude Control.Lens Week08.Lens> lars ^. address


Output:
Address {_city = "Regensburg"}
```

The & symbol here is function application, but the other way around - the argument comes first and then the function.
Again, we can compose.

```haskell
Prelude Control.Lens Week08.Lens> lars & address . city .~ "Munich"

Output:
Person {_name = "Lars", _address = Address {_city = "Munich"}}
```

There is another type of optics called Traversables, that zooms not only into one field, but into many simultaneously. If you had a list it would zoom into each element. So, for example, we could use a list of integers, with the each traversable that works with many container types, including lists, and set every element to 42.

```haskell
Prelude Control.Lens Week08.Lens> [1 :: Int, 3, 4] & each .~ 42

Output:
[42,42,42]
```

You may see a type-defaults warning when you run the above, but it is removed here.
A cool thing is that various types of lenses can be combined, again with the dot operator. For example

```haskell
Prelude Control.Lens Week08.Lens> iohk & staff . each . address . city .~ "Athens"

Output:
Company {_staff = [Person {_name = "Alejandro", _address = Address {_city = "Athens"}},Person {_name = "Lars", _address = Address {_city = "Athens"}}]}
```

And this is exactly what our goTo function achieved, so we can write ```goTo'``` as

```haskell
goTo' :: String -> Company -> Company
goTo' there c = c & staff . each . address . city .~ there
```

And this is actually what we did when we configured our test.

```haskell
tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
```

The function ```defaultCheckOptions``` is of type ```CheckOptions``` and there is a lens from ```CheckOptions``` to ```emulatorConfig```, and this is the part that we wanted to change.

That concludes our brief excursion into optics and lenses.


## Property Based Testing with QuickCheck


Property Based Testing is quite a revolutionary approach to testing that is much more powerful than simple unit testing. It originated from Haskell, which with its pureness and immutable data structures, is particularly suited to this approach. It has now been copied by almost all other programming languages.

### QuickCheck

One of the inventors of QuickCheck, which is the most prominent and was the first library using this approach, is John Hughes, who is also one of the original inventors of Haskell. He and his company work with IOHK to provide special support for this approach to testing Plutus contracts.
Before we look at using QuickCheck for Plutus contracts, let's first look at its use for pure Haskell programs.
Property based testing subsumes unit tests. Let's write a very simple and silly unit test.

```haskell
module Week08.QuickCheck where

prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)

-- Insertion sort code:

-- | Sort a list of integers in ascending order.
--
-- >>> sort [5,1,9]
-- [1,5,9]
--
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x $ sort xs

-- | Insert an integer at the right position into an /ascendingly sorted/
-- list of integers.
--
-- >>> insert 5 [1,9]
-- [1,5,9]
--
insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : y : ys
                 | otherwise    =  y : insert x ys

isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)

prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs

prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs
```

But before we go to that, let's first look at vanilla quick check for pure Haskell programs. In particular property-based testing subsumes unit tests, so unit tests are just a special case.

```haskell
prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)
```
After loading this module, and the Test.QuickCheck module, we can test our unit test in the repl.

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> quickCheck prop_simple

Output:
+++ OK, passed 1 test.
```

This is not very exciting. For a more interesting example, the same module contains a buggy implementation of an insertion sort.

```haskell
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x $ sort xs
```

Now to see a more interesting example, here's a implementation of a sort function, sorting list of integers. This is using insertion sort, which of course is not very efficient. 

The idea of insertion sort is you sort the tail of the list and then insert the head of the list at the right position. This is a buggy implementation.

```haskell
isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)
```

And in order to test it, for example, a property that we could test would be after applying sort to a list of integers; the resulting list is sorted.

```haskell
prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs
```

Using this, we can now provide a QuickCheck property that is not just simply of type Bool, but instead is a function from a list of Ints to Bool.

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> quickCheck prop_sort_sorts 

Output:
*** Failed! Falsified (after 8 tests and 4 shrinks):    
[0,0,-1]
```

It fails, and gives us an example where the property does not hold. We can test that example.

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> sort [0, 0, -1]

Output:
[0,-1]
```

And can see that, indeed, it is not correct.

How does QuickCheck do this? If you provide a function with one or more arguments, it will generate random arguments for the function. In our example, QuickCheck has generated 100 random lists of integers and, for each of those lists, has checked whether the property holds, until it hit a failure.

Note that the failure was reported as:

```haskell
*** Failed! Falsified (after 8 tests and 4 shrinks):    
```

This means that after 8 tests the property was falsified, but at this point, rather than just report the failure, it has tried to shrink it - to simplify it.

This is a powerful feature of QuickCheck, because the random counter examples that QuickCheck finds are very complicated - long lists with long numbers. But once a counter example has been found, QuickCheck tries to simplify it, perhaps by dropping some elements from the list, or by making some of the numbers smaller, until it doesn\'t find a way to get an even simpler example.

It is this combination of random test generation and shrinking that makes QuickCheck so tremendously useful.

We can see what type of random lists QuickCheck generates.

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> sample (arbitrary :: Gen [Int])

Output:
[]
[0]
[2,4,-1,3]
[3,-1,4,3,-5]
[3,-1,-8,-4,-6]
[4,5,-1,4,-7,2,8,4,-5]
[-8,-8,-11,-12,2,-4,-12,2,4]
[7,9,3,-5,5,-9,3,1,11]
[12,-7,-9,9,-11,-15,5,-10,-7,4,8,8,-12,-6,16]
[-11,11,-1,-6]
[14,2,-5,9,13,-8,-8,-17,-1,-11,-19,15,9,8,-19,-4,16,4,4,19]
```

The way QuickCheck does this random generation is by using a type class called Arbitrary

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> :i Arbitrary


Output:
type Arbitrary :: * -> Constraint
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
```

There are many more lines to the above output, but the important ones are shown. We can see that it has two methods. One is called arbitrary and one is called shrink.
```Gen``` is yet another monad. The monad provides various methods that allow for random number generation for values of type ```a```.
The second method is shrink, which, when given an a will provide a list of simpler versions of ```a```. This, of course, depends on the type of ```a```.
If we look at the output above that provides some random integer lists, we see something interesting. The further we go down the list, the more complicated the list becomes. The first is just the empty list, then we get single-element lists, then some longer lists, and it tends towards greater complexity over time.
In addition to just providing random generation in the ```Gen``` monad, there is also a concept of complexity. If you implement an instance of ```Gen``` you are expected not only to generate a random a but also a random a of some given complexity.
When QuickCheck checks a property, it starts with simple, random arguments, then makes them more complex over time. By default it tests 100 random arguments, but this can be configured.

Now that we know that our code fails, let’s try to fix it.

```haskell
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x xs
```

The problem is that all we do for a non-empty list is to insert the first element into the tail, but we don't recursively sort the tail.
Our first attempt to fix:

```haskell
sort :: [Int] -> [Int]
sort []     =  []
sort (x:xs) =  insert x $ sort xs
```

Now, when we test this:

```haskell
Prelude Control.Lens Test.QuickCheck> :r


Output:
Ok, one module loaded.

Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> quickCheck prop_sort_sorts 

Output:
+++ OK, passed 100 tests.
```

It passes. However, if we test specifically for the case that failed previously:

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> sort [0, 0, -1]

Output:
[-1,0]
```

It is clearly not correct. Even though the list has been sorted, the length of the list has changed. This leads to an important point. QuickCheck can't do magic - its results are only as good as the properties we provide. What we see here is that our property prop_sort_sorts is not strong enough to test if the function is correct.

We can add a second property that checks the length.

```haskell
prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs
```

And we find that this property is not satisfied by our code.

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> quickCheck prop_sort_preserves_length
*** Failed! Falsified (after 4 tests and 3 shrinks):    
[0,0]
```

The bug in our code is in the insert function.

```haskell
insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : ys
                 | otherwise    =  y : insert x ys
```

We say here that, if ```x``` is less or equal to ```y```, then we append ```x``` to ```ys```, but we have forgotten about the ```y```. It should read:

```haskell
insert x (y:ys)  | x <= y       =  x : y : ys
```

This should fix it:

```haskell
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> :r
Prelude Control.Lens Test.QuickCheck Week08.QuickCheck> quickCheck prop_sort_preserves_length
+++ OK, passed 100 tests.
```

Of course, this is still not proof that our function is correct, because these two properties are still not enough to specify a sorting function fully. For example, the sorting function could return a list of the same length containing only zeroes. This would pass all tests. It is quite an art to find properties to guarantee that, if they are all satisfied, there is no bug.

Even so, this approach to testing is often more effective than unit testing as it can test a huge number of random cases and can find examples of failure which a programmer writing a unit test may not have thought of.


## Property Based Testing of Plutus Contracts

Now that we have seen what QuickCheck can do, we will turn our attention to using it to test Plutus contracts.

Here we hit a problem - how do you use QuickCheck to test side-effected code? This problem does not only arise with blockchain, it arises with all systems that use IO.

John Hughes always uses the example of the file system. How would you test file system operations, i.e. reading, writing, opening and closing files, using QuickCheck.

The approach to use is very similar to the one you can use with Plutus. The idea is that you start with a model.


![testplutus1](https://user-images.githubusercontent.com/59018247/159126151-b887722b-8f5f-4b5e-850c-81c8cf20a436.png)


The model is basically an idealized model of how the real world system should work. There must be some sort of relation between the model and the real system.
If the real system is a file system, then you could, in the model, have an idealized version of how you think files should work. And then, what QuickCheck does, in its random generation, is to generate a random sequence of actions that you can perform on the system. In the example of a file system, it would randomly generate a sequence of opening files, closing files, writing to files, reading files and so on. Now you can basically step this model and the system in parallel.
You have some sort of action that you perform in the real world, and you apply the same type of action to your model. Then your real system has progressed into a new state, and your model has also been updated. After this step, you can compare the two and check that they are still in sync. You can then continue this for several steps.

![testplutus2](https://user-images.githubusercontent.com/59018247/159126177-7e7aa7b1-397d-4588-a9b0-5c70e6e7d5c5.png)


While our first QuickCheck example generated a random list of Ints, the idea for testing a real world system is to generate random lists of actions and then to apply those actions both to a model and to the real system and to check that the model and the real system stay in sync.

Shrinking in this example would be that, if you have a list of actions that show that there is a bug, then you can, for example, drop some of the actions and see whether the problem still arises. This can be repeated until you cannot drop any further actions from the list and still reproduce the bug.

This is exactly how the QuickCheck support for Plutus works. In order to test a Plutus contract, we have to come up with a model and define our expectations of how the various endpoints, for example, would change the model. We would then need to provide a link between the model and the real system (the emulator), and then apply the QuickCheck machinery.

The code to do this is in the module: test/Spec/Model.hs 

We notice that we import two Plutus test modules, with the QuickCheck support being provided by the ContractModel, which has all the machinery to define a model and to link it to a real contract.

```haskell
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
```

And we import three more test modules. One for Tasty, one for QuickCheck, and one that allows for using QuickCheck properties in Tasty test suites.

```haskell
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck  
```

To define a model, we first define a datatype that represents the state of one TokenSale instance.

```haskell
data TSState = TSState
  { _tssPrice    :: !Integer
  , _tssLovelace :: !Integer
  , _tssToken    :: !Integer
  } deriving Show
```

It has three fields - the current price, the current supply of lovelace in the contract, and the current supply of tokens in the contract.

Then our model ```TSModel``` is a map from wallets to ```TokenSale``` states.

```haskell
newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
deriving Show
```

The idea in this test is that we have two wallets and each wallet runs its own TokenSale contract, and the two wallets will trade different tokens.

We create lenses for the model. We need optics to interact with the ContactModel library.

```haskell
makeLenses ''TSModel
```

All the logic that defines how our model should behave, and how it is linked to the real contract is in

```haskell
instance ContractModel TSModel where
```

First we have an associated datatype. This is quite an advanced Haskell feature. In type classes, as well as methods, you can have data types. We have seen this before in validators where we define a dummy type that provides a link between the datum type and the redeemer type.

Here, we associate an Action type, which represents the actions that QuickCheck will generate. In principle, we just have one constructor in the Action type for each of the endpoints we saw earlier. We have additional arguments because now there are additional wallets at play and we must keep track of which wallet performs an action.

```haskell
data Action TSModel =
    Start Wallet
  | SetPrice Wallet Wallet Integer
  | AddTokens Wallet Wallet Integer
  | Withdraw Wallet Wallet Integer Integer
  | BuyTokens Wallet Wallet Integer
deriving (Show, Eq)
```

Start Wallet means that this wallet starts the token sale contract.

```SetPrice Wallet Wallet``` Integer means that the second wallet sets the price for the token sale contract operated by the first wallet. We know from the contract logic that this should only work if both the wallets are the same, because only the owner of the contract can set the price.

```AddTokens``` is very similar to ```SetPrice```.

For ```Withdraw```, the second wallet attempts to withdraw a certain number of lovelace and a certain number of tokens (respectively) from the token sale run by the first wallet. Again, this should fail if the two wallets are not the same.

In ```BuyTokens```, the second wallet will try to buy a certain number of tokens from the token sale run by the first wallet.

So, the Action type is the first ingredient.

The second ingredient is another associated datatype. For each instance of a contract that we are running, we want a key that identifies the instance.

```haskell
data ContractInstanceKey TSModel w s e where
  StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema' Text
  UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema    Text
```

This is a generalized, algebraic data type (GADT), so it's a little different to usual data declarations in Haskell. Instead of just providing the constructors, you provide the constructors with a type signature.

In ```ContractInstanceKey```, we have a constructor StartKey that takes a Wallet as an argument and then produces something of type

```haskell
ContractInstanceKey TSModel (Last TokenSale) TSStartSchema' Text
```

The point of GADTs is that with normal data types, the type parameters are the same for all constructors, for example, Action ```TSModel``` has five constructors, but the type is always ```TSModel```. But with GADTs, we are able to provide a more generalized type parameter - in this case ```TSModel w s e```.

We need this feature in this case because our contracts can have different type parameters.

There are two types of instances. Recall we have the start contract and the use contract, which have different type signatures.

```StartKey``` returns a type that consists of our model and then the parameters that come from the contract itself - the state type, the schema, and the error type. We used the primed version of ```TSStartSchema``` - ```TSStartSchema'``` because we don't want to create the NFT, we want to pass it in ourselves because it makes it easier to write the test if we know what NFT we will be using in advance.

We also provide a key for the use contract which takes two Wallets as parameters. The first is the one that owns the token sale that we are interacting with and the second is the one that actually runs the contract. As for the type parameters, there is no state parameter, and it uses a different schema - ```TSUseSchema```, but the error type is the same.

Next we need to provide the ```instanceTag``` method which, given an instance key and a wallet, will provide a so-called contract instance tag. As we already know the wallet that runs the instance, because that was one of the arguments to the instance key constructor we can ignore it as an argument.

```haskell
instanceTag key _ = fromString $ "instance tag for: " ++ show key
```

The ```instanceTag``` function doesn't have an accessible constructor, but it implements the IsString class. We haven't seen the IsString class explicitly but we have used it when we used the OverloadedStrings GHC extension - it allows a type class that implements it to be represented by a string literal. In particular, it has a method ```fromString``` which, given a string, will create an instance of the type.

The "instance tag for: " literal in the function above isn't necessary - all that is necessary is for the whole string to be unique for each instance that we will ever run in our tests.

There is a default implementation for the ```instanceTag``` method of the ```ContractModel``` class, so you normally don't have to implement it yourself. However, it only works if you have at most one contract instance per wallet. This is not the case for us, as we will have three instances per wallet - one start instance and two use instances (one for the own wallet's token sale, and one for the other wallet's token sale).

The next method that we need to implement is ```arbitraryAction``` which is how we tell the system how to generate a random action.

```haskell
arbitraryAction _ = oneof $
   (Start <$> genWallet) :
  [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
  [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
  [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
  [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]  
```

As an argument it gets the model state. We will come to this later, but we don't need it here and so ignore it in the method declaration.

The function ```oneof``` is one of the combinators provided by QuickCheck. Given a list of arbitrary actions, it randomly picks one of those.

Here we are using something else that we have not seen before - the applicative style. Recall that when we looked at monads, we saw that Monad has Applicative as a superclass. Applicative is often useful to write more compact monadic code.

First let's look at the ```genWallet``` function.

```haskell
genWallet :: Gen Wallet
genWallet = elements wallets
```

In the random generation monad ```Gen```, it generates a random wallet. It uses another combinator provided by QuickCheck, elements, which simply takes a list of the type that we wish to generate, and randomly picks one of those elements.

This is using another helper function wallets.

```haskell
wallets :: [Wallet]
wallets = [w1, w2]
```

Which, in turn, uses

```haskell
w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
```

So ```genWallet``` will randomly pick either Wallet 1 or Wallet 2.

Getting back to the ```arbitraryAction``` code:

```haskell
Start <$> genWallet
```

What this means is that we first use ```genWallet``` to generate a random wallet and then return Action Start w, where w is the wallet we have just picked.

The right-hand side is of type Gen Wallet and Start takes a Wallet and returns an action. If we fmap ```<$>``` this, we get a type of Gen Wallet -> Gen Action, which is what we want.

For the other four actions, we use an additional helper function ```genNonNeg``` which generates a nonnegative number.

```haskell
genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary
```

Now, when we want to generate a random action for ```SetPrice```, this is where the applicative style really shines.

```haskell
SetPrice <$> genWallet <*> genWallet <*> genNonNeg
```

If we wanted to write this in a do block, we would do something like

```haskell
w1 <- genWallet
w2 <- genWallet
p  <- genNonNeg
return (SetPrice w1 w2 p)
```

You can use the applicative style if the actions in the monad you are invoking don't depend on the result of previous actions. In a do block, you could do inspect the the result in w1 and make some choice based upon it. This is not possible in Applicative, but often monadic code doesn\'t make use of this power, and in these situations, we have this more compact way of writing it.

We can try out the arbitraryAction function in the repl.

```haskell
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> sample (arbitraryAction undefined :: Gen (Action TSModel))

Output:
Start (Wallet 1)
AddTokens (Wallet 1) (Wallet 1) 1
AddTokens (Wallet 1) (Wallet 1) 3
SetPrice (Wallet 1) (Wallet 2) 3
SetPrice (Wallet 1) (Wallet 1) 2
AddTokens (Wallet 1) (Wallet 1) 1
SetPrice (Wallet 2) (Wallet 1) 12
Withdraw (Wallet 2) (Wallet 1) 14 3
AddTokens (Wallet 2) (Wallet 1) 9
AddTokens (Wallet 2) (Wallet 1) 18
SetPrice (Wallet 2) (Wallet 1) 17
```

We see that is generates a sample of random actions with random arguments.

The next method to implement is ```initialState``` which, as the name suggests, is the initial state of our model.

```haskell
initialState = TSModel Map.empty
```

Now comes the most complex function that we must implement to set this up. You will recall from when we looked at the diagram that we must know what effect performing and action will have on the model. This is exactly what the ```nextState``` function does.

If we look at the type of ```nextState```, we see that it takes an action and returns something in yet another monad, this time the Spec monad. The Spec monad allows us to inspect the current state of our model, and also to transfer funds within our model.

```haskell
nextState :: ContractModel state => Action state -> Spec state ()
```

Let's look an example for Start. This should tell us the effect on our model if wallet w starts a token sale.

```haskell
nextState (Start w) = do
  withdraw w $ nfts Map.! w
    (tsModel . at w) $= Just (TSState 0 0 0)
  wait 1
```

Here we see a function from the Spec monad called ```withdraw```. Using ```withdraw``` means that some funds go from a wallet to a contract - it doesn't matter which contract. So, this says that the effect of Start will be that Wallet ```w``` loses the NFT.

The NFT is again something that is defined in a helper function. Let's quickly look at the helper functions that define the NFTs and tradable tokens.

Each wallet will trade its own token and each wallet will have its own NFT.

```haskell
tokenCurrencies, nftCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]
nftCurrencies   = Map.fromList $ zip wallets ["01", "02"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

nftAssets :: Map Wallet AssetClass
nftAssets = Map.fromList [(w, AssetClass (nftCurrencies Map.! w, nftName)) | w <- wallets]

nfts :: Map Wallet Value
nfts = Map.fromList [(w, assetClassValue (nftAssets Map.! w) 1) | w <- wallets]  
```

Wallet 1 will trade the A token and Wallet 2 will trade the B token. Wallet one will have the 01 NFT and Wallet two will have the 02 NFT.

While we are here, we can look at the ```tss``` helper which exists alongside the above helper functions and maps the wallets to their ```TokenSale``` parameters.

```haskell
tss :: Map Wallet TokenSale
tss = Map.fromList
    [ (w, TokenSale { tsSeller =  pubKeyHash $ walletPubKey w
                    , tsToken  =  tokens Map.! w
                    , tsNFT    =  nftAssets Map.! w
                    })
    | w <- wallets
    ]


Now, back to the ```nextState``` function. The first line of the do block says that the effect of calling Start will be that the wallet will loses the NFT to the contract. Remember that the NFT is locked in the contract when we start the token sale.

```haskell
nextState (Start w) = do
  withdraw w $ nfts Map.! w
    (tsModel . at w) $= Just (TSState 0 0 0)
  wait 1
```

Secondly, there will be an effect on the model state. Remember that the model state is a map from Wallet to ```TSState```, where ```TSState``` is a triple of price, tokens and Ada.

The second line of the do block says that after the contract has started, there will be an entry in the map at key w with 0 price, 0 tokens and 0 Ada.

The left hand side of the expression is another example of an optic, this time allowing us to access the map ```_tsModel``` from ```TSModel```. The at lens allows us to reference a map entry at a given key. The type returned by this optic is a Maybe as the key may or may not be there.

The ```$=``` comes from the Spec monad and it takes a lens on the left-hand side and then a new value on the right-hand side.

The wait function comes from the Spec monad and says here that the Start will take one slot.

Now we do something similar for all the other operations. Firstly, ```SetPrice```:

```haskell
nextState (SetPrice v w p) = do
  when (v == w) $
      (tsModel . ix v . tssPrice) $= p
  wait 1
```

In this function, we only do something if the wallet that invokes ```SetPrice``` is the same as the wallet that is running the token sale. If it is then the funds don't move, but we must update the model.

We use a different optic - instead of at we use ```ix``` which is a Traversal. It is similar to at, but whereas at returned a Maybe, ix does not. It also uses the ```tssPrice``` lens to access the first element of the ```TSState``` triple, which it sets to the price. In the event that ```ix``` does not find an entry, the line will have no effect.

Whether or not the wallets match, and whether or not the price update succeeds, we wait one slot.

The model state change for ```AddTokens``` is more complex.

```haskell
nextState (AddTokens v w n) = do
  started <- hasStarted v                                     -- has the token sale started?
  ...
```

First we check the the token sale for wallet ```v``` has actually started, and this is yet another helper function.

```haskell
getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v
```

Given a ```ModelState``` (which is of type ```TSModel``` but with additional information such as current funds and current time) and given a Wallet, we want to extract the ```TSState``` which is the state of the token sale contract for that wallet, which may or may not have started yet.

This is again performed using optics. There is a lens called ```contractState``` which, here is the ```TSModel``` type. We then zoom into the map and use the at lens, which will return Nothing if the wallet key ```v``` does not exist, or a Just ```TSState``` if it is there.

Using this, we can write a slight variety of this function which doesn't have the first argument. Instead it takes just the Wallet argument, but then returns the Maybe ```TSState``` in the Spec monad. In order to do that, we use a feature of the Spec monad, a function called ```getModelState```, which will return the model state, which we then pass to the primed version of the function along with the Wallet argument.

```haskell
getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v
```

And then another variation, this time called ```hasStarted```, which will tells us, within the Spec monad, whether the token sale has stared or not.

```haskell
hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v
```

This just checks whether the return value from ```getTSState v``` is a Just or a Nothing. The ```isJust``` function returns True if it is a Just, and we need to use fmap to lift it into the Spec monad.

Continuing the nextState function for ```AddTokens```:

```haskell
nextState (AddTokens v w n) = do
  started <- hasStarted v 
  when (n > 0 && started) $ do
    bc <- askModelState $ view $ balanceChange w
```

If the token sale has not started, we don't do anything because ```AddTokens``` shouldn't have any effect in that case.

We also check that the number of tokens to be added is greater than zero. If not, again we do nothing. Otherwise, we continue.

We now see another function from the Spec monad called ```askModelState```, which is similar to ```getModelState``` but it doesn't return the complete model state but instead takes a function and applies it to the the model state. The function ```view``` comes from the lens library and is just another name for the ```^.``` operator for viewing the result of zooming into a lens.

And there is a balanceChange ```w``` lens which is a lens to the balance change of wallet ```w```. The balance change refers to how much the funds of the wallet have changed since the start of the simulation.

At this point we have the balance change bound to bc. The reason we are doing this is because we want to make sure that the wallet has enough funds to add the requested number of tokens, which we now do. First we look up the token.

```haskell
let token = tokens Map.! v
```

Then we check whether the wallet has enough of them.

```haskell
when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
  withdraw w $ assetClassValue token n
  (tsModel . ix v . tssToken) $~ (+ n)
wait 1
```

The number in ```tokenAmt``` is the number of tokens the wallet had at the start, so by adding this to the balance change for the token, we get the number of tokens currently in the wallet.

If we have enough tokens, then we ```withdraw``` the correct number of tokens from the wallet, and we update the model to show that the tokens should now be in the contract. Note that instead of using ```$=``` to set the value, we use the ```$~``` function which applies a function to a value.

Again, we wait one slot.

Next, we write a ```nextState``` function for ```BuyTokens```:

```haskell
nextState (BuyTokens v w n) = do
when (n > 0) $ do
    m <- getTSState v
    case m of
        Just t
            | t ^. tssToken >= n -> do
                let p = t ^. tssPrice
                    l = p * n
                withdraw w $ lovelaceValueOf l
                deposit w $ assetClassValue (tokens Map.! v) n
                (tsModel . ix v . tssLovelace) $~ (+ l)
                (tsModel . ix v . tssToken)    $~ (+ (- n))
        _ -> return ()
wait 1
```

First we check the the number of tokens we are attempting to buy is positive. If so, then we get the state of the token sale.

If the state is a Just then we know that the token sale has started.

```haskell
m <- getTSState v
case m of
    Just t
```

If so, then we use optics to check that the number of tokens available in the contract is at least enough for us to buy what we are asking for.

```haskell
t ^. tssToken >= n -> do
```

If we are still going, we then lookup the current price and calculate how much the requested number of tokens will cost.

```haskell
let p = t ^. tssPrice
l = p * n
```

The effect should then be that our wallet loses that number of lovelace, and gains the tokens we buy. Here we see the deposit function for the first time. It is the opposite of the withdraw function.

```haskell
withdraw w $ lovelaceValueOf l
deposit w $ assetClassValue (tokens Map.! v) n
```

Finally we update the model state by adding the lovelace and removing the bought tokens.

```haskell
(tsModel . ix v . tssLovelace) $~ (+ l)
(tsModel . ix v . tssToken)    $~ (+ (- n))  
```

And we wait for one slot.

Finally, the ```Withdraw``` action:

```haskell
nextState (Withdraw v w n l) = do
when (v == w) $ do
    m <- getTSState v
    case m of
        Just t
            | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                (tsModel . ix v . tssLovelace) $~ (+ (- l))
                (tsModel . ix v . tssToken) $~ (+ (- n))
        _ -> return ()
wait 1  
```

This is only possible if the wallet wanting to withdraw is the same as the wallet running the sale. We check this first, then get the contract state.

We check both that there are enough tokens for us to withdraw the tokens that we are requesting, and also that there are enough lovelace for us to withdraw the lovelace that we are requesting. If this is satisfied, the effect is that we add the lovelace and the tokens to the wallet, and the model is updated to reflect the fact that the tokens and the lovelace have been removed.

That completes the nextState function declarations.

Right now, the model is just a conceptual model that has nothing to do with the contracts we wrote earlier. The names are suggestive because they have same names as we used in the redeemer, but there is no link yet between the model and the actual contracts.

The link is provided by yet another method in the ```ContractModel``` class that we have to implement, and that's the perform function.

```hakell
perform
  :: ContractModel state =>
    HandleFun state
    -> ModelState state
    -> Action state
    -> Plutus.Trace.Emulator.EmulatorTrace ()
```

It takes something called ```HandleFun``` and then it takes the ```ModelState``` and the ```Action```.

The ```HandleFun``` parameter gives us access to the contract handles.

Let's look at our implementation of this method. We don't need access to the ```ModelState``` for this example.

```haskell
Perform h _ cmd = case cmd of
  (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (nftCurrencies Map.! w, tokenCurrencies Map.! w, tokenNames Map.! w) >> delay 1
  (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                                    >> delay 1
  (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                                    >> delay 1
  (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                                    >> delay 1
  (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)   
```

Here we are linking actions to contract endpoints. Recall that we wrote functions that create keys that uniquely identify contracts. The functions were called ```StartKey``` and ```UseKey```.

The ```StartKey``` function takes one Wallet as an argument, and you can see that we give that argument here in the first line of the body of the function. Then we apply the function ```h``` to it. The function ```h``` is the ```HandleFun``` parameter and it is the job of this function to get a handle to the contract instance associated with a given key.

We also pass in the parameters. So in the example of the start action, we pass in a pre-computed values for the NFT, the token currencies and the token names. We will say later how the instanceSpec function links the ```StartKey``` to ```startEndpoint'```, the primed version of the function, which takes those three parameters.

The delay function used in perform is another simple helper function to wait for a number of slots.

```haskell
delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral
```

All the other actions are very similar, but note that they all use ```UseKey``` instead of ```StartKey```.

Finally, the last method we must provide for the ```ContractModel``` instance is precondition. This allows us to define the conditions under which it is acceptable to provide each action.

```haskell
precondition :: ContractModel state => ModelState state -> Action state -> Bool
```

The precondition for ```Start``` is that the token sale has not yet started. It says that, given a certain state ```s``` and the ```Start w``` action, check that the return value of ```getTSState' s w``` is Nothing.

```haskell
precondition s (Start w)          = isNothing $ getTSState' s w
```

And for the others, we do the opposite. They are only possible if the token sale has started.

```haskell
precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v  
```

One last thing, we must link the keys to actual contracts. We do this with the ```instanceSpec``` function.

```haskell
instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w startEndpoint' | w <- wallets] ++
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]  
```

The ```instanceSpec``` function returns a list of ```ContractInstanceSpec``` types.

A ```ContractInstanceSpec``` takes three arguments - the first is the key, the second is the wallet, and the third is the contract that is supposed to be invoked.

For the start endpoint, we generate a ```ContractInstanceSpec``` for each wallet.

For the use endpoint, we generate a ```ContractInstanceSpec``` for all combinations of two wallets. Note also that the ```useEndpoints``` function takes an argument of type ```TokenSale```, so we need to get this from Wallet ```v``` and pass it in.

And finally, we can define a QuickCheck property.

There's a function in Plutus.Contract.Test called ```propRunActionsWithOptions```:

```haskell
propRunActionsWithOptions
:: ContractModel state =>
   Plutus.Contract.Test.CheckOptions
   -> [ContractInstanceSpec state]
   -> (ModelState state -> Plutus.Contract.Test.TracePredicate)
   -> Actions state
   -> Property
```

First it takes the ```CheckOptions``` type that we have seen before when we did emulator trace testing. Next it takes the list of ```ContractInstanceSpecs``` that we defined above. Then it takes a function from ```ModelState``` to ```TracePredicate```, which allows us to insert additional tests. And finally, it produces a function from a list of ```Actions``` to ```Property```. ```Property``` is like a beefed-up Bool, which has additional capabilities, mostly for logging and debugging.

We use this in the ```prop_TS``` function. For options we use the same as before which allows us to specify the initial coin distributions. We give each wallet 1,000 Ada, the wallet's NFT and 1,000 of both tokens, A and B.

For the second argument we provide the ```instanceSpec``` function. For the third argument, we don't add any additional checks.

```haskell
prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]
```

This results in a type:

```haskell
Actions TSModel -> Property
```

And this is something that QuickCheck can handle.

Let's look at a sample of Actions ```TSModel```:

```haskell
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> sample (arbitrary :: Gen (Actions TSModel))

Output:
Actions []
Actions []
Actions []
Actions []
Actions []
Actions 
 [Start (Wallet 1),
  AddTokens (Wallet 1) (Wallet 2) 8,
  Withdraw (Wallet 1) (Wallet 2) 5 1,
  Withdraw (Wallet 1) (Wallet 1) 7 2,
  SetPrice (Wallet 1) (Wallet 1) 0,
  Start (Wallet 2),
  BuyTokens (Wallet 2) (Wallet 1) 2]
Actions 
 [Start (Wallet 1)]
Actions 
 [Start (Wallet 2),
  Withdraw (Wallet 2) (Wallet 1) 4 5,
  SetPrice (Wallet 2) (Wallet 2) 5,
  BuyTokens (Wallet 2) (Wallet 2) 3,
  Start (Wallet 1),
  BuyTokens (Wallet 1) (Wallet 1) 14,
  Withdraw (Wallet 1) (Wallet 1) 11 7,
  AddTokens (Wallet 2) (Wallet 1) 12]
Actions 
 [Start (Wallet 1),
  AddTokens (Wallet 1) (Wallet 2) 1,
  BuyTokens (Wallet 1) (Wallet 1) 11,
  SetPrice (Wallet 1) (Wallet 2) 5,
  Withdraw (Wallet 1) (Wallet 1) 10 6,
  Withdraw (Wallet 1) (Wallet 2) 13 0,
  BuyTokens (Wallet 1) (Wallet 1) 8,
  Withdraw (Wallet 1) (Wallet 2) 6 14,
  SetPrice (Wallet 1) (Wallet 2) 7,
  BuyTokens (Wallet 1) (Wallet 2) 4,
  AddTokens (Wallet 1) (Wallet 2) 3]
Actions 
 [Start (Wallet 1),
  BuyTokens (Wallet 1) (Wallet 2) 10]
Actions 
 [Start (Wallet 1),
  SetPrice (Wallet 1) (Wallet 2) 14,
  BuyTokens (Wallet 1) (Wallet 1) 20,
  BuyTokens (Wallet 1) (Wallet 2) 15,
  Start (Wallet 2),
  Withdraw (Wallet 2) (Wallet 2) 14 1,
  AddTokens (Wallet 2) (Wallet 2) 4,
  Withdraw (Wallet 2) (Wallet 1) 21 2,
  SetPrice (Wallet 2) (Wallet 1) 8,
  Withdraw (Wallet 1) (Wallet 2) 15 17,
  SetPrice (Wallet 1) (Wallet 1) 2,
  BuyTokens (Wallet 2) (Wallet 1) 4]
```

We notice here a similar pattern to before, where things start quite simply and get more complex as the list goes on.

So what will be tested? As we saw in the diagram back at the beginning, for all these randomly-generated action sequences, it will test that the properties we specified in the model - how the funds flow - corresponds to what actually happens in the emulator. If there is a discrepancy, the test will fail.

Let's use it:

```haskell
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> test

Output:
(21 tests)
```

It takes quite a while.

```haskell
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> test

Outpput:
(27 tests)
```

But it will run 100 if you let it complete.

What might be more interesting would be to implement a bug in the code and see if these tests will find it.

In the transition function of our ```TokenSale``` code, let’s forget to check that only the seller can change the price.

```haskell
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0 -> Just ( mempty -- Just ( Constraints.mustBeSignedBy (tsSeller ts)
    , State p $
      v <>
      nft (negate 1)
    )
...
```

We need to reload the code.

```haskell
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> :l test/Spec/Model.hs

Output:

Ok, one module loaded.
Prelude Test.QuickCheck Plutus.Contract.Test.ContractModel Spec.Model> test
*** Failed! Assertion failed (after 13 tests and 2 shrinks)...
```

You will see a whole bunch of output, but at the top, you will see clearly the action sequence that led to the bug.

```haskell
Actions
  [Start (Wallet 2),
   SetPrice (Wallet 2) (Wallet 1) 12,
   AddTokens (Wallet 2) (Wallet 2) 11,
   BuyTokens (Wallet 2) (Wallet 2) 1]
Expected funds of W2 to change by
   Value (Map [(02,Map [("NFT",-1)]),(bb,Map [("B",-10)])])
   (excluding 29466 lovelace in fees)
but they changed to
   Value (Map [(,Map [("",-12)]),(02,Map [("NFT",-1)]),(bb,Map [("B",-10)])])
Test failed.
```

And we see that Wallet 1 has tried to set the price of the token sale that was started by Wallet 2. This should result in no change, because Wallet 1 is not allowed to do this.

The model believes that the price should still be zero, but in the emulator the price has been set to 12.

Then Wallet 2 adds 11 tokens, and then buys 1 token from itself.

According to the model, the tokens should be free. The model expects that the wallet loses the NFT, that the wallet also loses 10 "B" because it gave 11 and then bought 1 back, and that there is no change in Ada in the wallet, because the token price is zero.

But in the emulator, setting the price did have an effect, and so it reports that the wallet lost 12 lovelace.

So the discrepancy in the flow of funds has been found, and QuickCheck reports the error.

By default this is all the QuickCheck test do. It only checks the flow of funds, whether the emulator and the model agree at each point. It is, however, possible to add additional checks. And it is also possible to influence the action sequences so that we can specify certain flows of actions to steer the tests in certain directions. That is called Dynamic Logic, and that is yet another monad.

Even though this is very powerful, it also has its limitations. For one, it only tests the contracts that we provide. It doesn't test all possible off-chain code. It is possible that some party could write their own off-chain code that would allow them to steal funds from our contract, and this QuickCheck model can't test for that.

The second problem is concurrency. We added this delay of one slot to each action to make sure that everything is nicely sequenced. Of course, in a real blockchain or in an emulator, wallets can have concurrent submissions of transactions. In principle we could try to do that with this model as well, but then we would need to somehow specify in the model what should happen in each case and that could get very complicated.

We should quickly look at how this integrates with Tasty.

There is a function in the Tasty library called ```testProperty``` that takes, as one its arguments, a QuickCheck property.

```haskell
tests :: TestTree
tests = testProperty "token sale model" prop_TS


You will see an additional stanza in this week's cabal file

test-suite plutus-pioneer-program-week08-tests
type: exitcode-stdio-1.0
main-is: Spec.hs
...
```

And, if we look at the referenced Spec.hs

```haskell
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.Model.tests
    ]  
```

We can see that it specifies a list of test modules. And these can be run from the command line with the following command:

```haskell
cabal test
```

## Homework


### Objective

    The object for the homework this week is to add an additional function to TokenSale.hs called Close called by the seller. It would be used by seller to close the contract, and collect the remaining tokens and lovelace in the contract.

### Solution

First, we can add ```Close``` to data ```TSRedeemer```:

```haskell
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer
```

Then, looking at  the state machine transition. We need to modify the ```Withdraw``` state(previous last state in old TokenSale.hs) to now handle giving the seller back the remaining tokens and ADA.

```haskell
   (v, Just p, Withdraw n l) | n >= 0 && l >= 0 &&
                                v `geq` (w <> toValue minAdaTxOut) -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             negate w
                                                                           )
      where
        w = assetClassValue (tsToken ts) n <>
            lovelaceValueOf l
    (_, Just _, Close)                                             -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State Nothing mempty
                                                                           )
    _                                                              -> Nothing
```

Here we do not require any inputs from the seller, as we are just returning the tokens. We do however require the signature from the seller.

Now we need to add the function close, which just accepts the ```TokenSale``` parameters:

```haskell
close :: TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close
```

Add close as an endpoint to ```TSUseSchema```:

```haskell
type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()
```

Then finally, add close to ```useEndpoints’``` :

```haskell
useEndpoints' :: ( HasEndpoint "set price" Integer s
                 , HasEndpoint "add tokens" Integer s
                 , HasEndpoint "buy tokens" Integer s
                 , HasEndpoint "withdraw" (Integer, Integer) s
                 , HasEndpoint "close" () s
                 )
              => TokenSale
              -> Promise () s Text ()
useEndpoints' ts = setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close'
  where
    setPrice'  = endpoint @"set price"  $ \p      -> handleError logError (setPrice ts p)
    addTokens' = endpoint @"add tokens" $ \n      -> handleError logError (addTokens ts n)
    buyTokens' = endpoint @"buy tokens" $ \n      -> handleError logError (buyTokens ts n)
    withdraw'  = endpoint @"withdraw"   $ \(n, l) -> handleError logError (withdraw ts n l)
    close'     = endpoint @"close"      $ \()     -> handleError logError (close ts)
```


The final code should look like:

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

module Week08.TokenSaleWithClose
    ( TokenSale (..)
    , TSRedeemer (..)
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    , useEndpoints'
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..))
import qualified Prelude

data TokenSale = TokenSale
    { tsSeller :: !PaymentPubKeyHash
    , tsToken  :: !AssetClass
    , tsTT     :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer
    | Close
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: TokenSale -> State (Maybe Integer) -> TSRedeemer -> Maybe (TxConstraints Void Void, State (Maybe Integer))
transition ts s r = case (stateValue s, stateData s, r) of
    (v, Just _, SetPrice p)   | p >= 0                             -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State (Just p) v
                                                                           )
    (v, Just p, AddTokens n)  | n > 0                              -> Just ( mempty
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             assetClassValue (tsToken ts) n
                                                                           )
    (v, Just p, BuyTokens n)  | n > 0                              -> Just ( mempty
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             assetClassValue (tsToken ts) (negate n) <>
                                                                             lovelaceValueOf (n * p)
                                                                           )
    (v, Just p, Withdraw n l) | n >= 0 && l >= 0 &&
                                v `geq` (w <> toValue minAdaTxOut) -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State (Just p) $
                                                                             v                                       <>
                                                                             negate w
                                                                           )
      where
        w = assetClassValue (tsToken ts) n <>
            lovelaceValueOf l
    (_, Just _, Close)                                             -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                                           , State Nothing mempty
                                                                           )
    _                                                              -> Nothing

{-# INLINABLE tsStateMachine #-}
tsStateMachine :: TokenSale -> StateMachine (Maybe Integer) TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsTT ts) (transition ts) isNothing

{-# INLINABLE mkTSValidator #-}
mkTSValidator :: TokenSale -> Maybe Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine (Maybe Integer) TSRedeemer

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @(Maybe Integer) @TSRedeemer

tsValidator :: TokenSale -> Validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient (Maybe Integer) TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

startTS :: AssetClass -> Contract (Last TokenSale) s Text ()
startTS token = do
    pkh <- Contract.ownPaymentPubKeyHash
    tt  <- mapErrorSM getThreadToken
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client (Just 0) mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts

setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l

close :: TokenSale -> Contract w s Text ()
close ts = void $ mapErrorSM $ runStep (tsClient ts) Close

type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
    .\/ Endpoint "close"      ()

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise
              $ endpoint @"start" $ startTS . AssetClass

useEndpoints' :: ( HasEndpoint "set price" Integer s
                 , HasEndpoint "add tokens" Integer s
                 , HasEndpoint "buy tokens" Integer s
                 , HasEndpoint "withdraw" (Integer, Integer) s
                 , HasEndpoint "close" () s
                 )
              => TokenSale
              -> Promise () s Text ()
useEndpoints' ts = setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' `select` close'
  where
    setPrice'  = endpoint @"set price"  $ \p      -> handleError logError (setPrice ts p)
    addTokens' = endpoint @"add tokens" $ \n      -> handleError logError (addTokens ts n)
    buyTokens' = endpoint @"buy tokens" $ \n      -> handleError logError (buyTokens ts n)
    withdraw'  = endpoint @"withdraw"   $ \(n, l) -> handleError logError (withdraw ts n l)
    close'     = endpoint @"close"      $ \()     -> handleError logError (close ts)

useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints = forever . awaitPromise . useEndpoints'
```

In order to test it we can update the Trace module to include it as well:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.TraceWithClose
    ( tests
    , runMyTrace
    ) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Test.Tasty

import           Week08.TokenSaleWithClose

tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token sale (with close) trace"
    (     walletFundsChange w1 (Ada.lovelaceValueOf   25_000_000  <> assetClassValue token (-25))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
    )
    myTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000 <> assetClassValue token 1000

tokenCurrency :: CurrencySymbol
tokenCurrency = "aa"

tokenName' :: TokenName
tokenName' = "A"

token :: AssetClass
token = AssetClass (tokenCurrency, tokenName')

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet w1 startEndpoint
    callEndpoint @"start" h (tokenCurrency, tokenName')
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet w1 $ useEndpoints ts
            h2 <- activateContractWallet w2 $ useEndpoints ts
            h3 <- activateContractWallet w3 $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"close" h1 ()
            void $ Emulator.waitNSlots 5
```

And model with close would look like:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.ModelWithClose
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                                 hiding (elements)
import           Control.Monad                                (forM_, void, when)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (isJust, isNothing)
import           Data.Monoid                                  (Last (..))
import           Data.String                                  (IsString (..))
import           Data.Text                                    (Text)
import           Plutus.Contract
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel           as Test
import           Plutus.Contract.Test.ContractModel.Symbolics
import           Plutus.Trace.Emulator                        as Trace
import           Ledger                                       hiding (singleton)
import           Ledger.Ada                                   as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Week08.TokenSaleWithClose                    (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints')

type TSUseSchema' = TSUseSchema .\/ Endpoint "init" TokenSale .\/ Endpoint "kill" ()

useEndpoints'' :: Contract () TSUseSchema' Text ()
useEndpoints'' = awaitPromise $ endpoint @"init" go
  where
    go :: TokenSale -> Contract () TSUseSchema' Text ()
    go ts = awaitPromise $ (useEndpoints' ts `promiseBind` \() -> go ts) `select` (endpoint @"kill" $ \() -> useEndpoints'')

data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

makeLenses ''TSState

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show

makeLenses ''TSModel

tests :: TestTree
tests = testProperty "token sale (with close) model" prop_TS

instance ContractModel TSModel where

    data Action TSModel =
              Start Wallet
            | SetPrice Wallet Wallet Integer
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
            | Close Wallet Wallet
        deriving (Show, Eq)

    data ContractInstanceKey TSModel w s e p where
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text ()
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema'  Text ()

    instanceWallet :: ContractInstanceKey TSModel w s e p -> Wallet
    instanceWallet (StartKey w) = w
    instanceWallet (UseKey _ w) = w

    instanceTag :: SchemaConstraints w s e => ContractInstanceKey TSModel w s e p -> ContractInstanceTag
    instanceTag key = fromString $ "instance tag for: " ++ show key

    arbitraryAction :: ModelState TSModel -> Gen (Action TSModel)
    arbitraryAction _ = oneof
        [ Start     <$> genWallet
        , SetPrice  <$> genWallet <*> genWallet <*> genNonNeg
        , AddTokens <$> genWallet <*> genWallet <*> genNonNeg
        , BuyTokens <$> genWallet <*> genWallet <*> genNonNeg
        , Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg
        , Close     <$> genWallet <*> genWallet
        ]

    initialState :: TSModel
    initialState = TSModel Map.empty

    initialInstances :: [StartContract TSModel]
    initialInstances =    [StartContract (StartKey v) () | v <- wallets]
                       ++ [StartContract (UseKey v w) () | v <- wallets, w <- wallets]

    precondition :: ModelState TSModel -> Action TSModel -> Bool
    precondition s (Start w)          = isNothing $ getTSState' s w
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v
    precondition s (Close v _)        = isJust    $ getTSState' s v

    nextState :: Action TSModel -> Spec TSModel ()
    nextState (Start w) = do
        wait 3
        (tsModel . at w) $= Just (TSState 0 0 0)
        withdraw w $ Ada.toValue minAdaTxOut
    nextState (SetPrice v w p) = do
        wait 3
        when (v == w) $
            (tsModel . ix v . tssPrice) $= p
    nextState (AddTokens v w n) = do
        wait 3
        started <- hasStarted v                                     -- has the token sale started?
        when (n > 0 && started) $ do
            bc <- actualValPart <$> askModelState (view $ balanceChange w)
            let token = tokens Map.! v
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                withdraw w $ assetClassValue token n
                (tsModel . ix v . tssToken) $~ (+ n)
    nextState (BuyTokens v w n) = do
        wait 3
        when (n > 0) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
    nextState (Withdraw v w n l) = do
        wait 3
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
    nextState (Close v w) = do
        wait 4
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t -> do
                    deposit w $ lovelaceValueOf (t ^. tssLovelace)               <>
                                assetClassValue (tokens Map.! w) (t ^. tssToken) <>
                                Ada.toValue minAdaTxOut
                    (tsModel . at v) $= Nothing
                _ -> return ()


    startInstances :: ModelState TSModel -> Action TSModel -> [StartContract TSModel]
    startInstances _ _ = []

    instanceContract :: (SymToken -> AssetClass) -> ContractInstanceKey TSModel w s e p -> p -> Contract w s e ()
    instanceContract _ (StartKey _) () = startEndpoint
    instanceContract _ (UseKey _ _) () = useEndpoints''

    perform :: HandleFun TSModel -> (SymToken -> AssetClass) -> ModelState TSModel -> Action TSModel -> SpecificationEmulatorTrace ()
    perform h _ m (Start v)         = do
        let handle = h $ StartKey v
        withWait m $ callEndpoint @"start" handle (tokenCurrencies Map.! v, tokenNames Map.! v)
        Last mts <- observableState handle
        case mts of
            Nothing -> Trace.throwError $ GenericError $ "starting token sale for wallet " ++ show v ++ " failed"
            Just ts -> forM_ wallets $ \w ->
                callEndpoint @"init" (h $ UseKey v w) ts
    perform h _ m (SetPrice v w p)   = withWait m $ callEndpoint @"set price"  (h $ UseKey v w) p
    perform h _ m (AddTokens v w n)  = withWait m $ callEndpoint @"add tokens" (h $ UseKey v w) n
    perform h _ m (BuyTokens v w n)  = withWait m $ callEndpoint @"buy tokens" (h $ UseKey v w) n
    perform h _ m (Withdraw v w n l) = withWait m $ callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)
    perform h _ m (Close v w)        = do
        withWait m $ callEndpoint @"close" (h $ UseKey v w) ()
        when (v == w) $ forM_ wallets $ \w' ->
            callEndpoint @"kill" (h $ UseKey v w') ()
        delay 1

withWait :: ModelState TSModel -> SpecificationEmulatorTrace () -> SpecificationEmulatorTrace ()
withWait m c = void $ c >> waitUntilSlot ((m ^. Test.currentSlot) + 3)

deriving instance Eq (ContractInstanceKey TSModel w s e p)
deriving instance Show (ContractInstanceKey TSModel w s e p)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v

getTSState :: Wallet -> Spec TSModel (Maybe TSState)
getTSState v = do
    s <- getModelState
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool
hasStarted v = isJust <$> getTSState v

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

genWallet :: Gen Wallet
genWallet = elements wallets

genNonNeg :: Gen Integer
genNonNeg = getNonNegative <$> arbitrary

tokenAmt :: Integer
tokenAmt = 1_000

prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig . initialChainState .~ Left d)
    defaultCoverageOptions
    (const $ pure True)
  where

    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS
```
