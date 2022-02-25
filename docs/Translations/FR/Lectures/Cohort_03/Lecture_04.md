Plutus Pioneer Program - Cohort 3, 
3 février 2022

Contributeur:
[Joe Totes](https://github.com/Totes5706)

Traduit par:
[Baptiste Bauer](https://github.com/bbauer02)

Lecture de la semaine de Lars Brünjes: [PPP-Cohort3-Lecture4](https://youtu.be/gxMW9uXTEj4)

La version Google Doc originale en Anglais [HERE](https://docs.google.com/document/d/1gRv1mZdzokLW_UHQzmCmSoW-ioDnj2gOkwxEbeMTjBs/edit#)

# Lecture 4: Monads


## Sommaire

- [Lecture 4: Monads](#lecture-4-monads)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 4](#preparation-for-lecture-4)
  - [Monads](#monads)
  - [The Emulator Trace Monad](#the-emulator-trace-monad)
  - [The Contract Monad](#the-contract-monad)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)

## Travail préparatoire pour la Lecture 4

Avant de débuter la lecture 4, nous devons en premier faire une mise à jour de notre environnement de développement. 

Vous pouvez copier et coller le code de ce guide directement dans votre terminal ou votre IDE.

En premier, allons dans le répertoire ``plutus-pionner-program`` afin de récupérer le contenu de la lecture de la semaine 4. 
Execute: 

```
$: ~/plutus-pioneer-program$ git pull
```

Nous pouvons maintenant naviguer dans le répertoire ``week004`` et ouvrir le fichier
``cabal.project``:


```
$: ~/plutus-pioneer-program/code/week04$ cat cabal.project
```

Récupérons le tag de la semaine dans le fichier `cabal.project file`, qui nous permettra de nous positionner sur la branche de ``plutus-apps`` :

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:ea1bfc6a49ee731c67ada3bfb326ee798001701a
```
Retournons ensuite dans le répertoire `plutus-apps` et mettons le à jour avec le tag ``git`` du jour:

```
$: ~/plutus-apps$ git checkout main
```
```
$: ~/plutus-apps$ git pull
```
```
$: ~/plutus-apps$ git checkout ea1bfc6a49ee731c67ada3bfb326ee798001701a
```

Nous devons être à jour maintenant, et nous pouvons lancer `nix-shell` dans ce répertoire.
Lançons donc la commande `nix-shell`:

```
$: ~/plutus-apps$ nix-shell
```

Revenons donc dans le répertoire ``week04`` pour commencer à lancer les commandes ``cabal``:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl
```
Si la procédure c'est bien déroulée, nous devons être prêt à commencer à suivre la lecture:

```
Ok, 9 modules loaded.
Prelude Week04.Contract> 
```

## Monads

Afin d'explorer de nouvelles classes Haskell, nous avons besoin de charger ``hello.hs``. Dans le ``nix-shell`` du répertoire ``week02``, exécutons*:

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


*note: si ``repl`` est déjà en cours d'exécution, nous pouvons retourner dans le ``nix-shell`` en pressant les boutons ``CTRL+Z``.

Nous avons étudié la classe IO Functor.

```haskell
class Functor f where
Plutus Tx version of Functor.
Methods
fmap :: (a -> b) -> f a -> f b
Plutus Tx version of fmap.
```

Prenons l'exemple d'une fonction qui mettrant en lettres capitales un texte. Importons en premier `Data.char`:

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


Opérateur (>>):

```haskell
(>>) :: Monad m => m a -> m b -> m b
```

```
*Main Data.Char> putStrLn "Hello" >> putStrLn "World"

Output:
Hello
World
```

Opérateur Bind (>>=):

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```
```
*Main Data.Char> getLine >>= putStrLn
<user input> Haskell

Output:
Haskell
```


Retour:

```haskell
return :: Monad m => a -> m a
```

```
*Main Data.Char> return "Haskell" :: IO String

Output:
Haskell
```

Une fonction IO plus compliquée dans ``hello.hs``:

```haskell
main :: IO ()
main = bar -- putStrLn "Hello, world!"

bar :: IO ()
bar = getLine >>= \s ->
      getLine >>= \t ->
      putStrLn (s ++ t)
```

Appel de la fonction ``bar``*:

```
*Main Data.Char> bar
<user input> one
<user input> two

Output:
onetwo
```

*note, Si vous vous trouvez à l'extérieur du ``REPL`` vous pouvez directelement lancer ``hello.hs`` de la sorte:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal run hello
Up to date
<user input> one
<user input> two

Output:
onetwo
```




Le type `Maybe`:

```haskell
data Maybe a
Constructors
Nothing
 
Just a
```

Exemple, **read**. Importons en premier ``Text.Read (readMaybe)``

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


``Read Maybe`` (Idéal, cela évite de retourner une exception):
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


Nous allons maintenant apprendre des utilisations plus complexes de ``readMaybe`` dans le ficher `Maybe.hs`. Quittez le repl avec ``CTRL+Z``, puis exécutez:

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl

Output:
Ok, 9 modules loaded.
Prelude Week04.Contract>
```


Maintenant, nous chargeons le fichier ``Maybe.hs`` :

```
Prelude Week04.Contract> :l src/Week4/Maybe.hs

Output:
Ok, two modules loaded.
Prelude Week04.Maybe>
```


Dans le fichier ``Maybe.hs``, nous examinons d'abord la fonction ``foo`` :

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

Exemples de sorties de la fonction ``foo`` :

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

Maintenant, regardons ``bindMaybe`` dans ``Maybe.hs`` (pour créer une version plus concise de la fonction ``foo`` appelée ``foo'`` ):

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

Exemples de sorties de la fonction ``foo'`` comme prévu :

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

Le type ``Either``:

```haskell
data Either a b
Constructors

Left a
 
Right b
```

Exemple avec le type ``Either``:

```
Prelude Week04.Maybe> Left "Haskell" :: Either String Int

Output:
Left "Haskell"

Prelude Week04.Maybe> Right 7 :: Either String Int

Output:
Right 7
```


Maintenant, nous chargeons le fichier ``Either.hs``:

```
Prelude Week04.Contract> :l src/Week04/Either.hs

Output:
Ok, two modules loaded.
Prelude Week04.Either>
```

À l'intérieur de ``Each.hs``, nous examinons d'abord la fonction ``readEither ``:

```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
   Nothing -> Left $ "can't parse: " ++ s
   Just a  -> Right a
```

Exemples de sorties de ``readEither ``:

```
Prelude Week04.Either> readEither "42" :: Either String Int

Output:
42

Prelude Week04.Either> readEither "42+x" :: Either String Int

Output:
Left "can't parse: 42+x"
```

Nous regardons ensuite la fonction ``foo`` :


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

Nous regardons ensuite ``foo'``(version plus concise de ``foo``):

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


Exemples de sorties de ``foo'`` comme prévu :

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

Enfin, nous chargeons le fichier ``Writer.hs`` :

```
Prelude Week04.Either> :l src/Week04/Writer.hs

Output:
Ok, two modules loaded.
Prelude Week04.Writer>
```

La première fonction d'intérêt dans le fichier est ``number`` :

```haskell
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]
```

Exemple d'utilisation de `number`:

```
Prelude Week04.Writer> number 42

Output:
Writer 42 ["number: 42"]
```

Maintenant, regardons `foo`:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
 let
   s = k + l + m
   Writer _ us = tell ["sum: " ++ show s]
 in
   Writer s $ xs ++ ys ++ zs ++ us
```


Exemple d'utilisation de `foo`
```
Prelude Week04.Writer> foo (number 1) (number 2) (number 3)

Output:
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
```

Maintenant examinons la fonction ``bindWriter`` avec ``foo’``:

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



Exemple d'utilisation de ``foo’``:
```
Prelude Week04.Writer> foo' (number 1) (number 2) (number 3)

Output:
Writer 6 ["number: 1","number: 2","number: 3","sum: 6"]
```

Dernièrement, nous avons vu la classe ``Monad`` dans son ensemble:

```haskell
Monad
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(>>) :: Monad m => m a -> m b -> m b
return :: Monad m => a -> m a
```

## Emulator Trace Monad

Avant de pouvoir commencer à utiliser **Emulator Trace Monad**, commençons par charger le ``repl``.

```
[nix-shell:~/plutus-pioneer-program/code/week04]$ cabal repl
```


Importons ``Plutus.Trace.Emulator`` et ``Data.Default``

```
Prelude Week04.Contract> import Plutus.Trace.Emulator

Prelude Plutus.Trace.Emulator Week04.Contract> import Data.Default

Prelude Plutus.Trace.Emulator Data.Default Week04.Contract>
```

Voici la configuration de Emulator:

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
Et ensuite voici l'instance de configuration par défaut : 

```haskell
Defined in Wallet.Emulator.Stream
Methods
def :: EmulatorConfig
```
Un exemple simple de la configuration par défaut de Emulator :

```
Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
def :: EmulatorConfig

Output:
EmulatorConfig {_initialChainState = Left (fromList [(Wallet 1bc5f27d7b4e20083977418e839e429d00cc87f3,Value (Map [(,Map [("",100000000)])])),(Wallet 3a4778247ad35117d7c3150d194da389f3148f4a,Value (Map [(,Map [("",100000000)])])),(Wallet 4e76ce6b3f12c6cc5a6a2545f6770d2bcb360648,Value (Map [(,Map [("",100000000)])])),(Wallet 5f5a4f5f465580a5500b9a9cede7f4e014a37ea8,Value (Map [(,Map [("",100000000)])])),(Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58,Value (Map [(,Map [("",100000000)])])),(Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491,Value (Map [(,Map [("",100000000)])])),(Wallet bdf8dbca0cadeb365480c6ec29ec746a2b85274f,Value (Map [(,Map [("",100000000)])])),(Wallet c19599f22890ced15c6a87222302109e83b78bdf,Value (Map [(,Map [("",100000000)])])),(Wallet c30efb78b4e272685c1f9f0c93787fd4b6743154,Value (Map [(,Map [("",100000000)])])),(Wallet d3eddd0d37989746b029a0e050386bc425363901,Value (Map [(,Map [("",100000000)])]))]), _slotConfig = SlotConfig {scSlotLength = 1000, scSlotZeroTime = POSIXTime {getPOSIXTime = 1596059091000}}, _feeConfig = FeeConfig {fcConstantFee = Lovelace {getLovelace = 10}, fcScriptsFeeFactor = 1.0}}
```


Lançons Emulator Trace:

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
Nous voyons ici que la sortie contient une quantité écrasante de données, correspondant aux événements de l'émulateur de la configuration par défaut. Ce n'est pas pratique, nous allons donc utiliser à la place un autre traceur d'événements de l'émulateur appelée ``runEmulatorTraceIO``.

Exécutons ``runEmulatorTraceIO``:

```haskell
runEmulatorTraceIO :: EmulatorTrace () -> IO ()
Runs the trace with runEmulatorTrace, with default configuration that prints a selection of events to stdout.
```

Exemple simple d'utilisation de ``runEmulatorTraceIO`` :

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

Lançons ``runEmulatorTraceIO’``

```haskell
runEmulatorTraceIO' :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
```
Comme vous pouvez le voir, ``RunEmulatorTraceIO' ``prend deux arguments supplémentaires. Vous pourriez potentiellement modifier la distribution initiale du portefeuille.

Où ``TraceConfig`` est :

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

Nous allons maintenant voir un exemple pratique dans le fichier ``Trace.hs``. Chargez le fichier ``Trace.hs`` :

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
:l src/Week04/Trace.hs

Output:
Ok, two modules loaded.
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Trace> 
```

Examinons le fichier ``Trace.hs``:

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

Utilisation le traçage avec la fonction ``test``:

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


## Le contrat Monad


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

Où ``W``: Autorise le contrat à écrire des messages de log de type ``w`` (Peut transmettre l'information à l'extérieur).

Où `S`: Spécifie quels points d'accès sont disponibles.

Où `E` : Spécifie le type des messages d'erreur.

Où `A`: Est le résultat


Tout d'abord, nous devons charger le fichier ``Contract.hs`` :

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Trace> 
:l src/Week04/Contract.hs

Output:
Ok, one module loaded.
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
```

En ouvrant le fichier ``Contract.hs``, nous allons d'abord apprendre à lever une erreur dans le contrat :

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



Nous pouvons tester le contrat 1 en appelant la fonction ``test1`` :

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test1

Output:
Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"BOOM!\""
```

Nous pouvons modifier ``myContract1`` afin de capturer l'exception à la place. ``Contract2`` est maintenant créé pour gérer l'exception :

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


Nous pouvons tester le contrat 2 en appelant la fonction ``test2`` :

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test2

Output:
Slot 00001: *** CONTRACT LOG: "caught: BOOM!"
```

En créant un nouveau contrat ``myContract3``, nous pouvons voir comment utiliser les points d'entrée dans le contrat :

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

Nous pouvons tester le contrat 3 en appelant la fonction ``test3`` :

```
Prelude Prelude Plutus.Trace.Emulator Data.Default Week04.Contract> 
test3

Output:
Receive endpoint call on 'foo' for Object XXX
Contract log: Number 42.0
Receive endpoint call on 'bar' for Object XXX
Slot 00001: *** CONTRACT LOG: "Haskell"
```

En créant un nouveau contrat ``myContract4``, nous pouvons voir comment faire attendre ‘n’ slots au contrat :

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

Nous pouvons tester le contrat 4 en appelant la fonction ``test4`` :

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

## Devoir maison partie 1

``` haskell
--Traduction de l'énoncé : 

-- Une trace qui appelle deux fois le point d'entrée de paiement de payContract sur le portefeuille 1, à chaque fois avec le portefeuille 2 comme destinataire, mais avec des montants donnés par les 2 arguments. Il devrait y avoir un délai de 1 slot apr-s chaque appel de point d'entrée
``` 

L'objectif est d'écrire une trace qui prend 2 paiements entiers et utilise le ``payContract`` pour exécuter deux paiements au portefeuille destinataire 2 avec un retard d'un slot.

Importer Wallet.Emulator.Wallet :
importer Wallet.Emulator.Wallet


Tout d'abord, nous devons passer deux valeurs dans ``paytrace`` (je les ai définies comme ``x`` , ``y`` respectivement):

```haskell
payTrace x y = do
```

Deuxièmement, nous devons appeler le ``payContract`` du portefeuille 1:

```haskell
h <- activateContractWallet (knownWallet 1) payContract
let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2
```
Maintenant, nous devons utiliser le point d'entrée  ``@pay`` et utiliser ``Payparams`` pour payer le bénéficiaire ``Wallet2`` avec la première valeur, `x` :

```haskell
callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = x
       }

```


Attendre 1 slot avant d'appeler le prochain paiement:

```haskell
void $ Emulator.waitNSlots 1
```

Maintenant, nous devons réutiliser le point de terminaison ``@pay`` et utiliser ``Payparams`` pour payer le bénéficiaire ``Wallet2`` avec la deuxième valeur, ``y`` :

```haskell
callEndpoint @"pay" h $ PayParams
       { ppRecipient = pkh
       , ppLovelace  = y
       }
```

Attendez 1 créneau après le deuxième paiement :
```haskell
void $ Emulator.waitNSlots 1
```

L'émulateur de trace final devrait ressembler à :

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


En exécutant ``payTest1``, nous obtenons le résultat suivant :

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

Le but du devoir 2 est de prendre en compte le cas où un portefeuille n'a pas suffisamment de fonds lors d'un transfert vers un deuxième portefeuille. Dans ``payTest2``, le premier paiement est supérieur au solde du portefeuille dans le ``portefeuille 1``. Nous devons donc gérer une erreur pour le premier paiement ``x``, tout en continuant le contrat à passer pour la valeur ``y``.

Nous devons d'abord examiner le ``payContract``, plus précisément :
```haskell
$ void $ submitTx tx
```
Tout d'abord, puisque nous allons utiliser la gestion des erreurs de décompression(unpack), nous devons l'importer dans le fichier :

```haskell
import Data.Text              (Text, unpack)
```
Nous pouvons maintenant modifier la ligne d'envoie de transaction ci-dessus pour gérer une erreur, créer un journal, puis poursuivre le contrat :

```haskell
handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
```

Le ``payContract`` final devrait ressembler à :

```haskell
payContract :: Contract () PaySchema Text ()
payContract = do
   pp <- awaitPromise $ endpoint @"pay" return
   let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
   handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ submitTx tx
   payContract
```
En exécutant ``payTest2``, nous obtenons le résultat suivant :

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
