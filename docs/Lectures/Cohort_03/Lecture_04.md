# Lecture 4: Emulator Trace and Contract Monads

Plutus Pioneer Program - Cohort 3 
February 3, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)

Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture4](https://youtu.be/gxMW9uXTEj4)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1gRv1mZdzokLW_UHQzmCmSoW-ioDnj2gOkwxEbeMTjBs/edit#)

# Lecture 4: Monads


## Table of Contents

- [Lecture 4: Monads](#lecture-4-monads)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 4](#preparation-for-lecture-4)
  - [Monads](#monads)
  - [The Emulator Trace Monad](#the-emulator-trace-monad)
  - [The Contract Monad](#the-contract-monad)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)

## Preparation for Lecture 4

Before we can get started in lecture 4, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.


First, head to the plutus-pioneer-program directory to grab the lecture week 4 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week04 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week04$ cat cabal.project
```

Grab the plutus-apps tag inside the cabal.project file:

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:ea1bfc6a49ee731c67ada3bfb326ee798001701a
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout ea1bfc6a49ee731c67ada3bfb326ee798001701a
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week04 folder to start running the cabal commands:
```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl
```
If successful,  you should now be ready to start the lecture:

```
Ok, 9 modules loaded.
Prelude Week04.Contract> 
```

## Monads


In order to explore some new Haskell classes, we need to load hello.hs. In the nix-shell in the week04 folder, *run:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl plutus-pioneer-program-week04:exe:hello

Output:

Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - plutus-pioneer-program-week04-0.1.0.0 (exe:hello) (ephemeral targets)
Preprocessing executable 'hello' for plutus-pioneer-program-week04-0.1.0.0..
GHCi, version 8.10.4.20210212: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( app/hello.hs, interpreted )
Ok, one module loaded.
*Main>
```


*note: If repl is already running you can hit CTRL+Z to return to nix-shell



We learned about the Functor IO class.

```haskell
class Functor f where
Plutus Tx version of Functor.
Methods
fmap :: (a -> b) -> f a -> f b
Plutus Tx version of fmap.
```

Example, upper case function. First import Data.Char:

```
*Main> import Data.Char


*Main Data.Char> toUpper 'q'

Output:
'Q'
```


```
*Main Data.Char> map toUpper "haskell"

Output:
"HASKELL"
```



```
*Main Data.Char> fmap (map toUpper) getLine 
<user input> Haskell

Output:
"HASKELL"
```


(>>) Operator:

```haskell
(>>) :: Monad m => m a -> m b -> m b
```

```
*Main Data.Char> putStrLn "Hello" >> putStrLn "World"

Output:
Hello
World
```

(>>=) Bind Operator:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```
```
*Main Data.Char> getLine >>= putStrLn
<user input> Haskell

Output:
Haskell
```


Return:

```haskell
return :: Monad m => a -> m a
```

```
*Main Data.Char> return "Haskell" :: IO String

Output:
Haskell
```


A more complicated IO function in hello.hs:

```haskell
main :: IO ()
main = bar -- putStrLn "Hello, world!"

bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)
```


Call the *bar function:

```
*Main Data.Char> bar
<user input> one
<user input> two

Output:
onetwo
```

*note, if you are outside the repl you can directly run hello.hs by:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal run hello
Up to date
<user input> one
<user input> two

Output:
onetwo
```





Maybe Type:

```haskell
data Maybe a
Constructors
Nothing
 
Just a
```


Example, read. First import Text.Read (readMaybe):

```
*Main Data.Char> import Text.Read (readMaybe)
```
```
*Main Data.Char Text.Read> read "42" :: Int

Output:
42
```
```
*Main Data.Char Text.Read> read "42+x" :: Int

Output:
*** Exception: Prelude.read: no parse
```


Read Maybe (more ideal, avoids throwing an exception):
```
*Main Data.Char Text.Read> readMaybe "42+x" :: Maybe Int

Output:
Nothing
```

```
*Main Data.Char Text.Read> readMaybe "42" :: Maybe Int

Output:
Just 42
```



We will now learn more intricate uses of readMaybe in the Maybe.hs file. Exit the repl with CTRL+Z, then execute:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl

Output:
Ok, 9 modules loaded.
Prelude Week04.Contract>
```

Now we load the Maybe.hs file:

```
Prelude Week04.Contract> :l src/Week4/Maybe.hs

Output:
Ok, two modules loaded.
Prelude Week04.Maybe>
```


Inside the Maybe.hs file, we first look at the foo function:

```haskell
foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
   Nothing -> Nothing
   Just k  -> case readMaybe y of
       Nothing -> Nothing
       Just l  -> case readMaybe z of
           Nothing -> Nothing
           Just m  -> Just (k + l + m)
```

Example outputs of the foo function:

```
Prelude Week04.Maybe> foo "1" "2" "3"

Output:
Just 6

Prelude Week04.Maybe> foo "" "2" "3"

Output:
Nothing

Prelude Week04.Maybe> foo "1" "2" ""

Output:
Nothing
```

Now we look at bindMaybe in Maybe.hs(to create a more concise version of the foo function called foo’ ):

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x

foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
            readMaybe y `bindMaybe` \l ->
            readMaybe z `bindMaybe` \m ->
            Just (k + l + m)
```

Example outputs of the foo’ function as expected:

```
Prelude Week04.Maybe> foo' "1" "2" "3"

Output:
Just 6

Prelude Week04.Maybe> foo' "" "2" "3"

Output:
Nothing

Prelude Week04.Maybe> foo' "1" "2" ""

Output:
Nothing
```

Either Type:

```haskell
data Either a b
Constructors

Left a
 
Right b
```

Example of Either Type:

```
Prelude Week04.Maybe> Left "Haskell" :: Either String Int

Output:
Left "Haskell"

Prelude Week04.Maybe> Right 7 :: Either String Int

Output:
Right 7
```


Now we load the Either.hs file:

```
Prelude Week04.Contract> :l src/Week04/Either.hs

Output:
Ok, two modules loaded.
Prelude Week04.Either>
```

Inside Either.hs, we first look at the readEither function:

```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
   Nothing -> Left $ "can't parse: " ++ s
   Just a  -> Right a
```

Example outputs of readEither:

```
Prelude Week04.Either> readEither "42" :: Either String Int

Output:
42

Prelude Week04.Either> readEither "42+x" :: Either String Int

Output:
Left "can't parse: 42+x"
```

We then look at the foo function:


```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
   Nothing -> Left $ "can't parse: " ++ s
   Just a  -> Right a

foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
   Left err -> Left err
   Right k  -> case readEither y of
       Left err -> Left err
       Right l  -> case readEither z of
           Left err -> Left err
           Right m  -> Right (k + l + m)
```

Example outputs of foo:

```
Prelude Week04.Either> foo "1" "2" "3"

Output:
Right 6

Prelude Week04.Either> foo "" "2" "3"

Output:
Left "can't parse: "

Prelude Week04.Either> foo "ays" "2" "3"

Output:
Left "can't parse: ays"

Prelude Week04.Either> foo "1" "2" "aws"

Output:
Left "can't parse: aws"
```

We then look at foo’ (more concise version of foo):

```haskell
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
            readEither y `bindEither` \l ->
            readEither z `bindEither` \m ->
            Right (k + l + m)
```


Example outputs of foo’ as expected:

```
Prelude Week04.Either> foo' "1" "2" "3"

Output:
Right 6

Prelude Week04.Either> foo' "" "2" "3"

Output:
Left "can't parse: "

Prelude Week04.Either> foo' "ays" "2" "3"

Output:
Left "can't parse: ays"

Prelude Week04.Either> foo' "1" "2" "aws"

Output:
Left "can't parse: aws"
```

Finally, we load the Writer.hs file:

```
Prelude Week04.Either> :l src/Week04/Writer.hs

Output:
Ok, two modules loaded.
Prelude Week04.Writer>
```

The first function of interest in the file is number:

```haskell
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]
```

Example of number:

```
Prelude Week04.Writer> number 42

Output:
Writer 42 ["number: 42"]
```

Now we look at foo:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
 let
   s = k + l + m
   Writer _ us = tell ["sum: " ++ show s]
 in
   Writer s $ xs ++ ys ++ zs ++ us
```


Example of foo:
```
Prelude Week04.Writer> foo (number 1) (number 2) (number 3)

Output:
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
```


Now we look at the bindWriter function with foo’:

```haskell
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
 let
   Writer b ys = f a
 in
   Writer b $ xs ++ ys

foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
            y `bindWriter` \l ->
            z `bindWriter` \m ->
            let s = k + l + m
            in tell ["sum: " ++ show s] `bindWriter` \_ ->
               Writer s []
```



Example of foo’:
```
Prelude Week04.Writer> foo' (number 1) (number 2) (number 3)

Output:
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
```
Lastly, we looked at the entire Monad Class:

```haskell
Monad
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>>) :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
```

## The Emulator Trace Monad


Before we can get started with using the Emulator Trace Monad, start by loading the repl.

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl
```


Import Plutus.Trace.Emulator and Data.Default

```
Prelude Week04.Contract> import Plutus.Trace.Emulator

Prelude Plutus.Trace.Emulator Week04.Contract> import Data.Default

Prelude Plutus.Trace.Emulator Data.Default Week04.Contract>
```

We are introduced to the Emulator Config:
```haskell
data EmulatorConfig
Constructors
EmulatorConfig
 
_initialChainState :: InitialChainState
State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
_slotConfig :: SlotConfig
Set the start time of slot 0 and the length of one slot
_feeConfig :: FeeConfig
Configure the fee of a transaction
```

Then the Default Emulator Config instance:

```haskell
Defined in Wallet.Emulator.Stream
Methods
def :: EmulatorConfig
```

Simple example of an Default Emulator Config:

```
Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
def :: EmulatorConfig

Output:
EmulatorConfig {_initialChainState = Left (fromList [(Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3,Value (Map [(,Map [("",100000000)])])),(Wallet 3a4778247ad35117d7c3150d194da389f3148f4a,Value (Map [(,Map [("",100000000)])])),(Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648,Value (Map [(,Map [("",100000000)])])),(Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8,Value (Map [(,Map [("",100000000)])])),(Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58,Value (Map [(,Map [("",100000000)])])),(Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491,Value (Map [(,Map [("",100000000)])])),(Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f,Value (Map [(,Map [("",100000000)])])),(Wallet c19599f22890ced15c6a87222302109e83b78bdf,Value (Map [(,Map [("",100000000)])])),(Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154,Value (Map [(,Map [("",100000000)])])),(Wallet d3eddd0d37989746b029a0e050386bc425363901,Value (Map [(,Map [("",100000000)])]))]), _slotConfig = SlotConfig {scSlotLength = 1000, scSlotZeroTime = POSIXTime {getPOSIXTime = 1596059091000}}, _feeConfig = FeeConfig {fcConstantFee = Lovelace {getLovelace = 10}, fcScriptsFeeFactor = 1.0}}
```


Run Emulator Trace:

```haskell
runEmulatorTrace :: EmulatorConfig -> EmulatorTrace () -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState)Source#
Run an emulator trace to completion, returning a tuple of the final state of the emulator, the events, and any error, if any.
```
```
Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
runEmulatorTrace def $ return ()

Output:
<pages of data>
```

We see here that the output contains an overwhelming amount of data, corresponding to the emulator events of the default configuration. This is not practical, so we will instead use another emulator trace called runEmulatorTraceIO.

Run Emulator Trace IO:

```haskell
runEmulatorTraceIO :: EmulatorTrace () -> IO ()
Runs the trace with runEmulatorTrace, with default configuration that prints a selection of events to stdout.
```

Simple example of Run Emulator TraceIO:

```
Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
runEmulatorTraceIO $ return ()

Output:

Final balances
Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3: 
    {, ""}: 100000000
Wallet 3a4778247ad35117d7c3150d194da389f3148f4a: 
    {, ""}: 100000000
Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648: 
    {, ""}: 100000000
Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8: 
    {, ""}: 100000000
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 100000000
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 100000000
Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f: 
    {, ""}: 100000000
Wallet c19599f22890ced15c6a87222302109e83b78bdf: 
    {, ""}: 100000000
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154: 
    {, ""}: 100000000
Wallet d3eddd0d37989746b029a0e050386bc425363901: 
    {, ""}: 100000000
```


Run Emulator TraceIO’

```haskell
runEmulatorTraceIO' :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
```
As you can see, Run Emulator TraceIO’ takes two additional arguments. You could potentially change the initial wallet distribution.

Where TraceConfig is:

```haskell
data TraceConfig
Options for how to set up and print the trace.
Constructors
TraceConfig
 
showEvent :: EmulatorEvent' -> Maybe String
Function to decide how to print the particular events.
outputHandle :: Handle
Where to print the outputs to. Default: stdout
```

We will now look at a practical example in the file Trace.hs. Load the Trace.hs file:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
:l src/Week04/Trace.hs

Output:
Ok, two modules loaded.
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Trace> 
```

Looking at the Trace.hs file:

```haskell
-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
   h1 <- activateContractWallet (knownWallet 1) endpoints
   h2 <- activateContractWallet (knownWallet 2) endpoints
   callEndpoint @"give" h1 $ GiveParams
       { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
       , gpDeadline    = slotToBeginPOSIXTime def 20
       , gpAmount      = 10000000
       }
   void $ waitUntilSlot 20
   callEndpoint @"grab" h2 ()
   s <- waitNSlots 2
   Extras.logInfo $ "reached " ++ show s
```

We can run a trace using the test function:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Trace> 
test

Output:
Receive endpoint call on 'give' for Object XXX

Slot 00002: *** CONTRACT LOG: "made a gift of 10000000 lovelace to XXX

Receive endpoint call on 'grab' for Object XXX

Slot 00022: *** USER LOG: reached Slot {getSlot = 22}
Slot 00022: *** CONTRACT LOG: "collected gifts"

Final balances
Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3: 
    {, ""}: 100000000
Wallet 3a4778247ad35117d7c3150d194da389f3148f4a: 
    {, ""}: 100000000
Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648: 
    {, ""}: 100000000
Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8: 
    {, ""}: 100000000
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 109995870
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 89999990
Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f: 
    {, ""}: 100000000
Wallet c19599f22890ced15c6a87222302109e83b78bdf: 
    {, ""}: 100000000
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154: 
    {, ""}: 100000000
Wallet d3eddd0d37989746b029a0e050386bc425363901: 
    {, ""}: 100000000
```


## The Contract Monad


```haskell
-- Contract w s e a
```
```haskell
newtype Contract w (s :: Row *) e aSource#
Contract w s e a is a contract with schema s, producing a value of type a or an error e. See note [Contract Schema].
Constructors
Contract
 
unContract :: Eff (ContractEffs w e) a
```


Where W: Allows contract to write log messages of type w (can pass information to the outside)

Where S: Specifies what endpoints are available

Where E: Specifies the type of error messages

Where A: Is the Result


First, we need to load the Contract.hs file:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Trace> 
:l src/Week04/Contract.hs

Output:
Ok, one module loaded.
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
```



Opening the file Contract.hs, we will first learn how to throw an error in the contract:

```haskell
myContract1 :: Contract () Empty Text ()
myContract1 = do
   void $ Contract.throwError "BOOM!"
   Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```


We can test out contract 1 by calling the test1 function:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test1

Output:
Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"BOOM!\""
```

We can modify myContract1 in order to instead catch the exception. Contract2 is now created to handle the exception:

```haskell
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
   (\err -> Contract.logError $ "caught: " ++ unpack err)
   myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2
```


We can test out contract 2 by calling the test2 function:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test2

Output:
Slot 00001: *** CONTRACT LOG: "caught: BOOM!"
```

Creating a new contract myContract3, we can see how to use endpoints in the Contract:

```haskell
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do
   awaitPromise $ endpoint @"foo" Contract.logInfo
   awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
   h <- activateContractWallet (knownWallet 1) myContract3
   callEndpoint @"foo" h 42
   callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3
```

We can test out contract 3 by calling the test3 function:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test3

Output:
Receive endpoint call on 'foo' for Object XXX
Contract log: Number 42.0
Receive endpoint call on 'bar' for Object XXX
Slot 00001: *** CONTRACT LOG: "Haskell"
```

Creating a new contract myContract4, we can see how to have the contract wait ‘n’ slots:

```haskell
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
   void $ Contract.waitNSlots 10
   tell [1]
   void $ Contract.waitNSlots 10
   tell [2]
   void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
   h <- activateContractWallet (knownWallet 1) myContract4

   void $ Emulator.waitNSlots 5
   xs <- observableState h
   Extras.logInfo $ show xs

   void $ Emulator.waitNSlots 10
   ys <- observableState h
   Extras.logInfo $ show ys

   void $ Emulator.waitNSlots 10
   zs <- observableState h
   Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
```

We can test out contract 4 by calling the test4 function:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test4

Output:
Slot 00007: *** USER LOG: []
Slot 00007: SlotAdd Slot 8
Slot 00017: *** USER LOG: [1]
Slot 00017: SlotAdd Slot 18
Slot 00027: *** USER LOG: [1,2]
```

## Homework Part 1

```haskell
-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
```


The goal homework 1 is to write an emulator trace that takes 2 integer payments, and utilizes the payContract to execute two payments to the recipient wallet 2 with a delay of one slot.
Import Wallet.Emulator.Wallet:
import Wallet.Emulator.Wallet


First, we need to pass two values into paytrace (I defined them as x , y respectively):
```haskell
payTrace x y = do
```

Second, we need to call the payContract from Wallet 1:
```haskell
h <- activateContractWallet (knownWallet 1) payContract
let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
```

Now, we need to use the @pay endpoint and use the Payparams to pay the beneficiary Wallet2 with the first value, x:
```haskell
callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = x
       }

```



Wait 1 slot before calling the next payment:
```haskell
void $ Emulator.waitNSlots 1
```

Now, we need to use the @pay endpoint again and use the Payparams to pay the beneficiary Wallet2 with the second value, y:
```haskell
callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = y
       }
```

Wait 1 slot after the second payment:
```haskell
void $ Emulator.waitNSlots 1
```

The final trace emulator should look like:

```haskell
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
   h <- activateContractWallet (knownWallet 1) payContract
   let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
   callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = x
       }
   void $ Emulator.waitNSlots 1
   callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = y
       }
   void $ Emulator.waitNSlots 1
```


Running payTest1, we get the following result:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Homework> 
payTest1

Output:
Final balances
Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3: 
    {, ""}: 100000000
Wallet 3a4778247ad35117d7c3150d194da389f3148f4a: 
    {, ""}: 100000000
Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648: 
    {, ""}: 100000000
Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8: 
    {, ""}: 100000000
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 130000000
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 69999980
Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f: 
    {, ""}: 100000000
Wallet c19599f22890ced15c6a87222302109e83b78bdf: 
    {, ""}: 100000000
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154: 
    {, ""}: 100000000
Wallet d3eddd0d37989746b029a0e050386bc425363901: 
    {, ""}: 100000000
```


## Homework Part 2


The goal of homework 2 is to account for the case where a wallet has insufficient funds to pass to the second wallet. In payTest2, the first payment is larger than the wallet balance in wallet 1. Therefore we need to handle an error for the first payment x, while still continuing the contract to pass for value y.

We need to first look at the payContract, more specifically:
```haskell
$ void $ submitTx tx
```

First, since we will be using the unpack error handling, we need to import it to the file:
```haskell
import Data.Text              (Text, unpack)
```

Now we can modify the submit transaction line above to handle an error, create a log, and then continue the contract:
```haskell
handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
```

The final payContract should look like:
```haskell
payContract :: Contract () PaySchema Text ()
payContract = do
   pp <- awaitPromise $ endpoint @"pay" return
   let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
   handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
   payContract
```

Running payTest2, we get the following result:

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Homework> 
payTest2

Output:
Slot 00001: *** CONTRACT LOG: "caught error: WalletError (InsufficientFunds

Final balances
Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3: 
    {, ""}: 100000000
Wallet 3a4778247ad35117d7c3150d194da389f3148f4a: 
    {, ""}: 100000000
Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648: 
    {, ""}: 100000000
Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8: 
    {, ""}: 100000000
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 120000000
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 79999990
Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f: 
    {, ""}: 100000000
Wallet c19599f22890ced15c6a87222302109e83b78bdf: 
    {, ""}: 100000000
Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154: 
    {, ""}: 100000000
Wallet d3eddd0d37989746b029a0e050386bc425363901: 
    {, ""}: 100000000
```
