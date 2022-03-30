# How to use Plutus-Starter to build your own Plutus Project on Cardano

Contributed By: [Joe Totes](https://github.com/Totes5706)

## Introduction

I will be using my NFT project as an example: [https://github.com/Totes5706/cardano-alonzo-nft-creator](https://github.com/Totes5706/cardano-alonzo-nft-creator). We will use the [Plutus-Starter](https://github.com/input-output-hk/plutus-starter) package to create a Cardano project that can be built as a standalone project using nix.

I used a fake example name "NFT-Maker" just to make this tutorial, however my real project name is cardano-alonzo-nft-creator.

## Table of Contents

- [How to use Plutus-Starter to build your own Plutus Project on Cardano](#how-to-use-plutus-starter-to-build-your-own-plutus-project-on-cardano)
  - [Table of Contents](#table-of-contents)
  - [1: Create the Template](#1-create-the-template)
  - [2: Import Haskell Files into the Project](#2-import-haskell-files-into-the-project)
  - [3: Modify the Cabal Files](#3-modify-the-cabal-files)
  - [4: Optional Add Cardano Node and CLI into Nix Shell](4-optional-add-cardano-node-and-cli-into-nix-shell)
  - [5: Build and Run Project](5-build-and-run-project)

## 1: Create the Template

Head to the plutus-start github to get started:

[https://github.com/input-output-hk/plutus-starter](https://github.com/input-output-hk/plutus-starter)

![Screenshot 2022-03-29 at 16-12-42 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160698741-00a59420-e4a1-40b5-b112-024836ee8695.png)

**Click on the green "Use this template" button in the top right corner of the page to clone the template into your own personal repo:**


![Screenshot 2022-03-29 at 16-12-12 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160698703-a983ccbc-7f6f-4acf-ab11-1cb615556430.png)


**Select a name for your new Cardano project/repo:**

![Screenshot 2022-03-29 at 16-13-25 Build software better together](https://user-images.githubusercontent.com/59018247/160698849-2758b939-63df-40e5-ba17-af1b392789ea.png)

**Remove the examples, scripts, and pab directories; they will be replaced with out project files**

![Screenshot 2022-03-29 at 21-57-02 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735287-c8d71878-b634-4e4e-bbfd-72f0fbbe0453.png)

![Screenshot 2022-03-29 at 22-49-48 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160741219-2947a154-8b11-4eba-a21b-4525232a9cef.png)

![Screenshot 2022-03-29 at 21-57-28 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735333-c449ea3e-8ce7-43e2-a2de-51e56d151387.png)

## 2: Import Haskell Files into the Project

**Now it is time to import my files into the project. It is good practice to have at least two main folders; an app directory for executable files and an src directory for the onchain and utility files.**

**My project has two executable files, a utilty file, and an onchain haskell file. First we will add the executable (main :: io()) files and create a directory called app:**

![Screenshot 2022-03-30 at 14-19-44 Totes5706_cardano-alonzo-nft-creator A bash script that will make real NFTs using a Haskell_Plutus on-chain validator on the Cardano Blockchain](https://user-images.githubusercontent.com/59018247/160904254-72c483e9-9821-404c-b71b-94bd7cd8c44c.png)

![Screenshot 2022-03-30 at 14-20-03 Totes5706_cardano-alonzo-nft-creator A bash script that will make real NFTs using a Haskell_Plutus on-chain validator on the Cardano Blockchain](https://user-images.githubusercontent.com/59018247/160904315-c7a140df-1303-42a3-b90a-ffeaa6a23797.png)

### token-policy.hs

![Screenshot 2022-03-29 at 22-41-08 Editing NFT-Maker_token-policy hs at main · Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740210-509736f5-e4f2-45e0-8e85-262bd30d35a7.png)

### token-name.hs

![Screenshot 2022-03-29 at 22-42-25 Editing NFT-Maker_token-name hs at main · Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740328-1a53c7a2-2271-49b9-a3cb-d7ef034d5d89.png)


![Screenshot 2022-03-29 at 22-42-49 Editing NFT-Maker_token-name hs at main · Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740375-bfd351da-4317-40b9-80b0-b806cb0eb47c.png)


**Now we can create the src folder that will contain the onchain code and utility functions that will get called by the executable:**

### Utils.hs

![Screenshot 2022-03-29 at 22-45-56 Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740783-70ad82ee-8be6-492a-968b-3c5dd8e16367.png)

### Token.Onchain.hs

![Screenshot 2022-03-29 at 22-46-28 Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740860-441276dd-e817-415b-8ce4-4fa5acd768d9.png)

![Screenshot 2022-03-29 at 22-47-45 Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160740989-0f24e9ab-4917-4e19-88fb-e56bfe3f5744.png)

**Lastly, I will be importing an env file for the node cardano socket, and  also my make-nft.bash script.**

![Screenshot 2022-03-29 at 22-53-26 Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160741618-aea8bd7f-cb06-42ee-bf21-0295f9e83356.png)

![Screenshot 2022-03-29 at 22-53-43 Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160741654-afd32749-7b27-4a77-8407-901f2aefd1df.png)

**Important to note here, my project uses a bash script to call the cabal exectuable files in this project. If you are creating a pure haskell/plutus project, you will not need a script to start your project. You would instead called cabal exec or cabal run depending on the project structure.**

**Example inside my bash script:**
```
...
#Send these three parameters to the on-chain code of Token.Onchain.hs to validate, then create the policy for the NFT
cabal exec token-policy $policyFile $oref $tn
...
```

## 3: Modify the Cabal Files

**Open the plutus-starter.cabal file, we can start by renaming this to our project:**

![Screenshot 2022-03-29 at 21-59-07 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735487-193dc04b-510b-45a0-86a3-7a5aa6a7b746.png)

**Change the name to your project name at the top**

![Screenshot 2022-03-29 at 21-59-48 Editing NFT-Maker_NFT-Maker cabal at main · Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160735578-3479ce69-a92a-4ad6-8c8f-b2be2e0085b7.png)

**Change the name, the author and the maintainer in this file:** 

![Screenshot 2022-03-29 at 22-01-46 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735767-c992dae8-4d60-42a2-a915-658ef85d83ec.png)

**Example:**

```haskell
cabal-version:      2.4
name:               NFT-Maker
version:            0.1.0.0

-- A short (one-line) description of the package.
-- synopsis:

-- A longer description of the package.
-- description:

-- A URL where users can report bugs.
-- bug-reports:

license: Apache-2.0
license-files: LICENSE
author:             Joe Totes
maintainer:         totinj@gmail.com
...
```
**Scroll to the bottom, we will be editing the Haskell files being linked here with our own:** 

![Screenshot 2022-03-29 at 22-08-39 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160736548-57c98335-c1fb-4129-a970-f24251e64e21.png)

**Example of my NFT project with comments:**

```haskell

library
  {- Include this to import the ghc-options and language above -}
  import: lang
  
  {- This is the directory location where our onchain code files will go, and also a utility function for my project -}
  hs-source-dirs:      src     

  {- These are the file names that will get called by our executable files listed below -}
  exposed-modules:     Token.OnChain
                       Utils     
                       
  {- These are the imports needed for the files Token.Onchain and Utils. Always include  base >= 4.9 && < 5 for Haskell. -}                     
  build-depends:       base >= 4.9 && < 5
                     , aeson
                     , bytestring
                     , cardano-api
                     , containers
                     , data-default
                     , freer-extras
                     , openapi3
                     , playground-common
                     , plutus-contract
                     , plutus-ledger
                     , plutus-ledger-api
                     , plutus-tx-plugin
                     , plutus-tx
                     , plutus-use-cases
                     , serialise
                     , text

{- This is the first executable file that will get called from the cabal exec command, and will be the main :: IO () files for our project -} 
executable token-policy

{- Include this to import the ghc-options and language above -}
  import: lang
  
{- This is the file name token-policy.hs, one of the executable files -}
  main-is: token-policy.hs
  
{- This is the local directory location of token-policy.hs -} 
  hs-source-dirs:      app

{- Always include this -} 
  ghc-options:         -threaded
   
{- My executable files have no imports, therefore I only need to include base >= 4.9 && < 5, and the project name  -}  
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker


{- This is the second executable file that will get called from the cabal exec command, and will be the main :: IO () files for our project -} 
executable token-name

{- Include this to import the ghc-options and language above -}
  import: lang

{- This is the file name token-name.hs, a second executable file -}
  main-is: token-name.hs
  
{- This is the local directory location of token-name.hs -}   
  hs-source-dirs:      app
  
{- Always include this -} 
  ghc-options:         -threaded
  
{- My executable files have no imports, therefore I only need to include base >= 4.9 && < 5, and the project name  -} 
  build-depends:       base >= 4.9 && < 5
                    , NFT-Maker
```

**Example without comments**

```haskell
library
  import: lang
  hs-source-dirs:      src
  exposed-modules:     Token.OnChain
                       Utils                   
  build-depends:       base >= 4.9 && < 5
                     , aeson
                     , bytestring
                     , cardano-api
                     , containers
                     , data-default
                     , freer-extras
                     , openapi3
                     , playground-common
                     , plutus-contract
                     , plutus-ledger
                     , plutus-ledger-api
                     , plutus-tx-plugin
                     , plutus-tx
                     , plutus-use-cases
                     , serialise
                     , text

executable token-policy
  import: lang
  main-is: token-policy.hs
  hs-source-dirs:      app
  ghc-options:         -threaded
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker

executable token-name
  import: lang
  main-is: token-name.hs
  hs-source-dirs:      app
  ghc-options:         -threaded
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker
```

**Once that is saved, open up cabal.project and edit the packages field to now include the cabal file named we just saved:**

![Screenshot 2022-03-29 at 22-35-32 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160739594-027e9325-87aa-46ac-a6ce-38cf9ff49d5f.png)

![Screenshot 2022-03-29 at 22-36-03 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160739649-642f8490-961e-4612-b176-90a8bfc544f5.png)

**Example**

```haskell
index-state: 2021-08-14T00:00:00Z

packages: NFT-Maker.cabal

-- You never, ever, want this.
write-ghc-environment-files: never

-- Always build tests and benchmarks.
tests: true
benchmarks: true
...
```
## 4: Optional Add Cardano Node and CLI into Nix Shell

**By default, the Plutus-Starter package does not include the Cardano-Node and CLI inside the nix-shell when you build the project. After some reverse engineering, this is what needs to be added to the shell.nix file:**

![Screenshot 2022-03-30 at 14-28-56 Totes5706_cardano-alonzo-nft-creator A bash script that will make real NFTs using a Haskell_Plutus on-chain validator on the Cardano Blockchain](https://user-images.githubusercontent.com/59018247/160905874-c43debbb-6ee1-49f6-bcec-97d86853c473.png)

The original shell.nix looks like this:

![Screenshot 2022-03-30 at 14-30-34 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160906147-3f1bf743-1651-4ea3-a14e-aecbc18f6325.png)

We will modify it to now include the Cardano-Node and CLI by changing to this:

```haskell
{ pure ? false
, source-repo-override ? { } }:
let
  packages = import ./. { inherit source-repo-override; };
  inherit (packages) pkgs plutus-apps plutus-starter;
  inherit (plutus-starter) haskell;
  
  cardano-node = import
   (pkgs.fetchgit {
     url = "https://github.com/input-output-hk/cardano-node";
     # A standard release compatible with the cardano-wallet commit above is always preferred.
     rev = "1.34.1";
     sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
   })
   { };

in
  haskell.project.shellFor {
    withHoogle = false;
    
    
    nativeBuildInputs = with plutus-starter; [
      hlint
      cabal-install
      cardano-node.cardano-cli
      cardano-node.cardano-node
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
```


## 5: Build and Run Project

**Now that everything is complete, we can build the project in nix-shell. First clone it locally:**


- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/Totes5706/cardano-alonzo-nft-creator.git
```

- Directory: ```totinj@penguin:~/cardano-alonzo-nft-creator$```
```
nix-shell
```
**Note: If you did step 4 properly, you should have access to cardano-node and CLI. Check it here**

- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
cardano-node --version

cardano-node 1.34.1 - linux-x86_64 - ghc-8.10
git rev 0000000000000000000000000000000000000000

```

- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
cardano-cli --version

cardano-cli 1.34.1 - linux-x86_64 - ghc-8.10
git rev 0000000000000000000000000000000000000000

```

**Now it should be ready to update and build:**

- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
cabal update
```
```
cabal build
```

**Now you call your script or cabal executable here**
