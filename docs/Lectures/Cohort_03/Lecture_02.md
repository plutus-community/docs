Plutus Pioneer Program - Cohort 3
January 20, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)

Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture2](https://youtu.be/BEr7lcCPjnA)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1wclIpwHW-Lo8R8IJHbjvEYjyzsm5qLUMcrN9bpCOL00/edit#)


# Lecture 2: Low and High Level Validation Scripts


## Table of Contents
- [Lecture 2: Low and High Level Validation Scripts](#lecture-2-low-and-high-level-validation-scripts)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 2](#preparation-for-lecture-2)
  - [Low Level Untyped Validation Scripts](#low-level-untyped-validation-scripts)
  - [High Level Typed Validation Scripts](#high-level-typed-validation-scripts)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)

## Preparation for Lecture 2

Before we can get started in lecture 2, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 2 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week02 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week02$ cat cabal.project
```

 Grab the plutus-apps tag inside the cabal.project file:
 
```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:6aff97d596ac9d59460aab5c65627b1c8c0a1528
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 6aff97d596ac9d59460aab5c65627b1c8c0a1528
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week02 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal repl
```

If successful,  you should now be ready to start the lecture:

```haskell
Ok, 9 modules loaded.
Prelude week02.Burn > 
```

## Low Level Untyped Validation Scripts



This lecture will be focused on the on-chain code of a plutus script. There are three pieces of data that a Plutus script recieves:

	1. The datum sitting at the UTxO
	2. The redeemer coming from the input and validation
	3. The context of the transaction being validated from its I/O

These three pieces of data need to be represented by a Haskell data type. Looking at the low level implementation, the same data type will be used for all three pieces of data. In the next section, we will look at high level validation which will look at custom data types for the datum and redeemer. High level validation will come at a cost to performance.


Looking at the data for a redeemer:

```haskell
data Data
A generic "data" type.
The main constructor Constr represents a datatype value in sum-of-products form: Constr i args represents a use of the ith constructor along with its arguments.
The other constructors are various primitives.
Constructors
Constr Integer [Data]
 
Map [(Data, Data)]
 
List [Data]
 
I Integer
 
B ByteString
``` 

We can now use the repl to get some hands-on experience. First, let's import PlutusTx

```haskell
Prelude week02.Burn > import PlutusTx
```

Now we can get information about data using the command:

```haskell
Prelude PlutusTx week02.Burn > :i Data

Output:
type Data :: *
data Data
  = Constr Integer [Data]
  | Map [(Data, Data)]
  | List [Data]
  | I Integer
  | B bytestring
```


Example :

```haskell
Prelude PlutusTx week02.Burn > I 42

Output:
I 42
```

```haskell
Prelude PlutusTx week02.Burn > :t I 42

Output:
I 42 :: Data
```


We can now use this extension (-XOverloadedStrings) in order to use literal strings for other string-like types. One example is the Byte string type. Execute:

```haskell
Prelude PlutusTx week02.Burn > :set -XOverloadedStrings
```

Example using the B constructor:

```haskell
Prelude PlutusTx week02.Burn > B "Haskell"

Output:
B "Haskell"
```

```haskell
Prelude PlutusTx week02.Burn > :t B "Haskell"

Output:
B "Haskell" :: Data
```

Example using Map:

```haskell
Prelude PlutusTx week02.Burn > 
:t Map [(I 42, B "Haskell"), (List [I 0], I 1000)]

Output:
Map [(I 42, B "Haskell"), (List [I 0], I 1000)] :: Data
```


With this knowledge, we can now create our first validator. We will be using the Gift.hs file included in the week02 folder.

Looking at validation part of Gift.hs:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

This is the most basic validator function. The file is called a gift because if anyone sends funds to this script address, then anyone else can consume that output to use.


We first look at mkValidatorScript:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

We have a haskell function that has the logic,  where the (||) Oxford brackets convert that to a syntactical representation of that function. The compiler takes that representation and turns it into a corresponding plutus core function. Then the ($$) takes that Plutus core and splices it into the source code. That result is what then turns into the validator.  

Where mkValidatorScript is:

```haskell
mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
```

To make use of this we need to add a Pragma to out make validator function:

```haskell
{-# INLINABLE mkValidator #-}
```

Now we can load this file in the repl:

```haskell 
Prelude PlutusTx week02.Burn > :l src/Week02/Gift.hs

Output:
Ok, one module loaded.
```

Make sure both PlutusTx and Ledger.Scripts are imported:

```haskell
Prelude week02.Gift > import PlutusTx
```
```haskell
Prelude PlutusTx week02.Burn > import Ledger.Scripts
```

Type validator:

```haskell
Prelude PlutusTx Ledger.Scripts week02.Gift > validator

Output:
Validator { <script> }
```

Where Validator and script are:

```haskell
newtype Validator
Validator is a wrapper around Scripts which are used as validators in transaction outputs.
Constructors
Validator
 
getValidator :: Script
```

```haskell
newtype Script
A script on the chain. This is an opaque type as far as the chain is concerned.
Constructors
Script
 
unScript :: Program DeBruijn DefaultUni DefaultFun ()
```

We can now run unScript $ getValidator validator:

```haskell
Prelude PlutusTx Ledger.Scripts week02.Gift >
unScript $ getValidator validator

Output:
Program () (Version () 1 0 0) (Apply () (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 5}))))))) (Delay () (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 1}))))) (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 1}))))
```

This is the plutus core script in this representation. We compiled our mkValidator function and turned it into Plutus Core.

The other two important parts of the validator are the validatorHash and ScriptAddress functions.

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Where valHash stores the hash of the validator and scrAddress stores the address of the script.

Example:

```haskell
Prelude PlutusTx Ledger.Scripts week02.Gift > valHash

Output:
67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656
```

```haskell
Prelude PlutusTx Ledger.Scripts week02.Gift > scrAddress

Output:
Address {addressCredential = ScriptCredential 67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656, addressStakingCredential = Nothing}
```

We can now test this in Plutus Playground.


In order to get started with Plutus Playground, we need to have two terminals running, both of which are in the nix-shell.

Let’s get started with terminal 1. Head to the plutus-apps directory and first run nix-shell:

```haskell
Terminal 1
totinj@penguin:~/plutus-apps$ nix-shell
```

Next we head to plutus-playground-server directory and run: 

```haskell
Terminal 1
[nix-shell:~/plutus-apps/plutus-playground-server]$ plutus-playground-server
```

If Successful, you will see the output:

```haskell
Terminal 1
Interpreter Ready
```

Let’s get started with terminal 2. Head to the plutus-apps directory and first run nix-shell:

```haskell
Terminal 2
totinj@penguin:~/plutus-apps$ nix-shell
```

Next we head to plutus-playground-client directory and run: 

```haskell
Terminal 2
[nix-shell:~/plutus-apps/plutus-playground-client]$ npm run start
```

If Successful, you will see the output:

```haskell
Terminal 2
[wdm]: Compiled successfully.

or

[wdm]: Compiled with warnings.
```

Keep both terminals open, and we should now be able to access Plutus Playground from the browser.

Open a browser and head to the address:

```haskell
https://localhost:8009
```

You will get a warning complaining about it being a risky website, ignore the message to click through anyway.

You should now be able to successfully compile and run the gift contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”

Our wallet setup should look like:<br/>

![Screenshot 2022-02-18 9 49 58 AM](https://user-images.githubusercontent.com/59018247/154706878-d148e873-28d3-44a0-b7f6-91002212f047.png)

<br/>
Genesis Slot 0 looks like:<br/>

![Screenshot 2022-02-17 4 11 38 PM](https://user-images.githubusercontent.com/59018247/154707925-7b82fbb3-5782-4d4a-90d3-524f676d3625.png)

<br/>
Slot 1, TX 0:<br/>

![Screenshot 2022-02-17 4 12 07 PM](https://user-images.githubusercontent.com/59018247/154707978-fe5c81d6-bbbc-4f02-a15a-e50d4e17a6dc.png)
 
<br/>
Slot 1, TX 1:<br/>

![Screenshot 2022-02-17 4 12 37 PM](https://user-images.githubusercontent.com/59018247/154708031-bd31ad20-1a0b-42ea-adde-0d9fdc2cc85b.png)

<br/>
Slot 2, TX 0:<br/>

![Screenshot 2022-02-17 4 13 01 PM](https://user-images.githubusercontent.com/59018247/154708054-94460dac-1b76-4b70-882f-a80be6200564.png)

<br/>
Final Balances:<br/>

![Screenshot 2022-02-17 4 14 07 PM](https://user-images.githubusercontent.com/59018247/154708099-31f7a939-0b37-4883-b1cc-eed6965d3ca1.png)

<br/>
We now look at the file Burn.hs where mkValidator looks like:

```haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = traceError "BURNT!"
```

Load the file and check for errors:

```haskell
Prelude PlutusTx week02.Gift > :l src/Week02/Burn.hs

Output:
Ok, one module loaded.
```

You should now be able to successfully compile and run the burn contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”: 


Evaluating the wallets with the same configuration as gift.hs: <br/>

![Screenshot 2022-02-18 9 49 58 AM](https://user-images.githubusercontent.com/59018247/154706878-d148e873-28d3-44a0-b7f6-91002212f047.png)

<br/>
Genesis Slot 0 looks like:<br/>

![Screenshot 2022-02-17 4 30 58 PM](https://user-images.githubusercontent.com/59018247/154708702-432431b5-68d3-47f5-b206-077d3cc267e2.png)

<br/>
Slot 1, TX 0:<br/>

![Screenshot 2022-02-17 4 31 46 PM](https://user-images.githubusercontent.com/59018247/154708737-0d4e40bd-eeff-4f59-827d-1921e1c67f5e.png)

<br/>
Slot 1, TX 1:<br/>

![Screenshot 2022-02-17 4 32 16 PM](https://user-images.githubusercontent.com/59018247/154708796-637a153f-4d6f-4e9b-95e5-52c3bd6315ff.png)

<br/>
Final Balances:<br/>

![Screenshot 2022-02-17 4 32 43 PM](https://user-images.githubusercontent.com/59018247/154708834-0bfad52f-5ad9-4d6f-a43f-554c0c397287.png)
<br/><br/>
As expected, the grab did not work. No transactions can ever use those outputs as inputs.

```haskell
Contract instance stopped with error: "WalletError (ValidationError (ScriptFailure (EvaluationError [\"BURNT!\"] \"CekEvaluationFailure\")))" ]
```
<br/><br/>
## High Level Typed Validation Scripts


We will now take a look at some examples of high level typed validation scripts. We can start by looking at Typed.hs:

```haskell
Prelude PlutusTx week02.Burn > :l src/Week02/Typed.hs

Output:
Ok, one module loaded.
```


The mkValidator function inside Typed.hs looks like:

```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```
This redeemer will check to see if the integer amount is 42, otherwise it will return false, outputting “wrong redeemer”.

We then modified the compilation function:

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @Integer
```

We first declare DatumType as type unit () and RedeemerType as an Integer. We then add a wrap function to be able to translate the strong types from the low level version. It is then declared in the where, that the datum and redeemer is  of type () and Integer respectively.

We can now look at a practical example in Plutus Playground. First let’s check to make sure there are no errors in the file isData.hs.

```haskell
Prelude PlutusTx week02.Typed > :l src/Week02/isData.hs

Output:
Ok, one module loaded.
```

Looking at the on-chain validating code:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator _ (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MySillyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MySillyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

You should now be able to successfully compile and run the isData contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”: 

Our first test case will use a grab value of 100. This should be expected to fail and the money should not be transfered.<br/>

![Screenshot 2022-02-17 4 49 26 PM](https://user-images.githubusercontent.com/59018247/154709233-12d4b818-b489-436f-98fa-7a6fe119f632.png)

<br/>
Results:<br/>

![Screenshot 2022-02-17 4 51 26 PM](https://user-images.githubusercontent.com/59018247/154709551-cec0e32e-5dcb-4b2b-bc32-2b477933c3a1.png)

<br/>
 As expected, the grab did not happen.
 
Our second test case will use a value that is 42. This should be expected to pass validation.<br/>

![Screenshot 2022-02-17 4 54 54 PM](https://user-images.githubusercontent.com/59018247/154709587-76539d7b-afdc-4430-867a-da76df4f6107.png)

<br/>
Results:<br/>

![Screenshot 2022-02-17 4 56 22 PM](https://user-images.githubusercontent.com/59018247/154709620-3f99f922-36d7-4a33-abf3-73812d9cf279.png)

<br/>
As expected, the grab was a success and the money was transfered.

## Homework Part 1

```haskell
-- This should validate if and only if the two Booleans in the redeemer are equal!

mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator _ _ _ = True -- FIX ME!
```

The goal of homework part 1 is to have the mkValidator pass only if the two booleans in the redeemer are equal.
First, we need to pass the correct parameters into mkValidator. It accepts a unit type (), followed by two booleans we can call b and c respectively.

```haskell
mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator () (b, c) _ = traceIfFalse "wrong redeemer" $ b == c
```

Next, we check whether the b and c are equal in value; otherwise throw the message “wrong redeemer”.
Then, we need to declare the data types for both unit and boolean parameters.

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = (Bool, Bool)
```

Next, we write the compilation code for a high level validation script, wrapping both the unit type and the boolean values.

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @(Bool, Bool)
```


Lastly, we write the boiler plate code for the validator, valHash, and srcAddress.

```haskell
validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```


The final on-chain code should look like:

```haskell
{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator () (b, c) _ = traceIfFalse "wrong redeemer" $ b == c

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = (Bool, Bool)

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @(Bool, Bool)

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Testing the code in Plutus Playground:<br/>

![Screenshot 2022-02-17 5 16 23 PM](https://user-images.githubusercontent.com/59018247/154709818-073df3c1-5fbc-4d22-9295-dccd7a95bc0d.png)

<br/>
Results:<br/>

![Screenshot 2022-02-17 5 17 58 PM](https://user-images.githubusercontent.com/59018247/154710014-dbbdda57-53f7-4599-ac2d-9c2ef5522f79.png)

<br/>
As expected, validation passed when both booleans were equal in value.

## Homework Part 2


The goal of homework part 2 is the same objective as part, with the exception of using custom data types for the redeemer:

```haskell
data MyRedeemer = MyRedeemer
   { flag1 :: Bool
   , flag2 :: Bool
   } deriving (Generic, FromJSON, ToJSON, ToSchema)
```

The logic is the same, except now we will be using MyRedeemer to pass both flags as booleans.

```haskell
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator () (MyRedeemer b c) _ = traceIfFalse "wrong redeemer" $ b == c
```

We alter the code and change the data typed from boolean to now MyRedeemer:

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MyRedeemer
```

Same change inside the compilation wrapper:

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MyRedeemer
```

The final on-chain code should look like:

```haskell
data MyRedeemer = MyRedeemer
   { flag1 :: Bool
   , flag2 :: Bool
   } deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator () (MyRedeemer b c) _ = traceIfFalse "wrong redeemer" $ b == c

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Testing the code in Plutus Playground:<br/>

![Screenshot 2022-02-17 5 34 57 PM](https://user-images.githubusercontent.com/59018247/154710339-65c5d8d1-cc63-4f59-bce7-963b9034ef8b.png)

<br/>
Results:<br/>

![Screenshot 2022-02-17 5 37 22 PM](https://user-images.githubusercontent.com/59018247/154710370-40c74548-2d13-48f5-9417-68265cb9d98d.png)

<br/>
As expected, validation passed when both booleans were equal in value
