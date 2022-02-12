# Lecture #5 - Minting Tokens and NFTs

Contributed by: Joe Totes
## Table of Contents
- [Lecture #5 - Minting Tokens and NFTs](#lecture-5---minting-tokens-and-nfts)
	- [Table of Contents](#table-of-contents)
	- [Preparation for Lecture 5](#preparation-for-lecture-5)
	- [Values](#values)
	- [A Simple Minting Policy](#a-simple-minting-policy)
	- [A More Realistic Minting Policy](#a-more-realistic-minting-policy)
	- [NFT's](#nfts)
	- [Homework Part 1](#homework-part-1)
	- [Homework Part 2](#homework-part-2)
## Preparation for Lecture 5

Before we can get started in lecture 5, we first must get our development environment up to date. You can right click on any of the code snippet images and click alt text to copy and paste the code into the terminal.

First, head to the plutus-pioneer-program directory to grab the lecture week 5 contents. Execute:
![0](../img/Lecture5/image54.png)



You can now navigate to the current week05 directory and open the cabal.project file:
![1](../img/Lecture5/image55.png)



Grab the plutus-apps tag:
![2](../img/Lecture5/image6.png)

Head back to  to the plutus-apps directory and update it to the  current git tag:
![3](../img/Lecture5/image26.png)


![4](../img/Lecture5/image10.png)


![5](../img/Lecture5/image35.png)

You should now be up to date and can run nix-shell in this directory. Run nix-shell:
![6](../img/Lecture5/image17.png)

Head back to the week05 folder to start running the cabal commands:
![7](../img/Lecture5/image29.png)


![8](../img/Lecture5/image14.png)

If successful,  you should now be ready to start the lecture:
![9](../img/Lecture5/image42.png)

## Values

We first looked at a new constructor Value:
![10](../img/Lecture5/image13.png)

Each native token, including ADA, is represented by a currency symbol and token name. Where currency symbol is:
![11](../img/Lecture5/image7.png)

Token name is:
![12](../img/Lecture5/image49.png)

Asset Class is
![13](../img/Lecture5/image64.png)

ADA will be one asset class. Custom native tokens will be other asset classes.


Starting with the repl, we can first import Plutus.V1.Ledger.Value and Plutus.V1.Ledger.Ada:
![14](../img/Lecture5/image27.png)

Then, we can set -XOverloadedStrings:
![15](../img/Lecture5/image28.png)

We first look at adaSymbol, adaToken, and lovelaceValueof:
![16](../img/Lecture5/image45.png)

![17](../img/Lecture5/image30.png)

![18](../img/Lecture5/image53.png)

Example:
![19](../img/Lecture5/image47.png)

Combining Values:
![20](../img/Lecture5/image46.png)

Then we learned how to use the function singleton that allows us to create values with native tokens:
![21](../img/Lecture5/image61.png)

Example:
![22](../img/Lecture5/image4.png)

Combining Values:
![23](../img/Lecture5/image15.png)

We can then take the value of the result using:
![24](../img/Lecture5/image12.png)

Example, where x is the input from the above entry:
![25](../img/Lecture5/image44.png)

We can then flatten the map using the flattenValue function:
![26](../img/Lecture5/image11.png)

Example:
![27](../img/Lecture5/image25.png)

## A Simple Minting Policy

Before we look at a simple minting script, we can review the relevant script context.

![28](../img/Lecture5/image24.png)


![29](../img/Lecture5/image40.png)


![30](../img/Lecture5/image5.png)

We can now load the Free.hs minting script.
![31](../img/Lecture5/image63.png)

You can now run curSymbol to get the hash of the script:
![32](../img/Lecture5/image62.png)

Looking at the off chain code:
![33](../img/Lecture5/image48.png)

Then finally, the Emulator Trace:
![34](../img/Lecture5/image32.png)

We can now run the test emulator trace:

![35](../img/Lecture5/image41.png)

![36](../img/Lecture5/image56.png)
Note how both wallets have the same hash associated with “ABC”.

## A More Realistic Minting Policy

Instead of having an unparameterized minting policy, we will change it to a parametrized one. This will instead allow an owner from a specific public key hash to mint, rather than anyone.

![37](../img/Lecture5/image20.png)

Where txSignedBy and scriptContextTxInfo are:
![38](../img/Lecture5/image38.png)

![39](../img/Lecture5/image58.png)

And the modified off chain code to account for the PaymentPubKeyHash:
![40](../img/Lecture5/image50.png)

Load the new Signed.hs file into the repl:
![41](../img/Lecture5/image1.png)

Run the test emulator Trace:

![42](../img/Lecture5/image19.png)

The Results:
![43](../img/Lecture5/image16.png)

The wallet’s now have different hashes associated with “ABC”.

## NFT's

Non Fungible Tokens are tokens which only exist once. Previous examples were not NFTs because we were able to mint as many tokens as possible.

Since the Mary era, it was possible to implement pseudo NFTs based using deadlines to lock down the minting process. This requires checking with a blockchain explorer whether or not one was minted before the deadline. These are not true NFTs since they require secondary checks.

Since the plutus era, we can construct true NFTs that are only minted once, without the need to validate from a blockchain explorer. The trick is to use a unique ID that cannot be duplicated; and in this case for Cardano, it is the UtxOs that only exist once. UtxOs are never reused.



![44](../img/Lecture5/image51.png)


![45](../img/Lecture5/image3.png)


![46](../img/Lecture5/image33.png)

## Homework Part 1

![47](../img/Lecture5/image22.png)

The goal of homework 1 is to write a Mary era contract that uses deadlines and signature checks to mint a specific token ABC.

We first need to implement the mkPolicy that takes the PaymentPubKeyHash, POSIXTime and ScriptContext to produce a Boolean to check both cases in which the beneficiary has signed the transaction; as well as checking that the deadline has not passed.

![48](../img/Lecture5/image18.png)

I created && logic that checks both the signature in the checkSig function and the deadline in the checkDeadline function. This will only return true if both are true. In checkDeadline, we also want to use “to” making sure we are in the valid range.

We then need to create the policy that takes both PaymentPubKeyHash and POSIXTime as pkh and deadline respectively.
![49](../img/Lecture5/image21.png)

Finally, we need to get the hash for curSymbol taking in both PaymentPubKeyHash and POSIXTime.
![50](../img/Lecture5/image43.png)

The final Code looks like:
![51](../img/Lecture5/image59.png)

The final output running the test trace looks like:
![52](../img/Lecture5/image34.png)


![53](../img/Lecture5/image60.png)


![54](../img/Lecture5/image57.png)

## Homework Part 2

![55](../img/Lecture5/image23.png)

The goal of homework part 2 is to mint an NFT for a given UTxO where the TokenName is a ByteString.

First we need to write the mkPolicy in which checks if the UTxO is consumed, and also if the correct amount was minted.

The "hasUTx0" function is a boolean that checks the TxOutRef to report true or false.

The checkMintedAmount will check to make sure only 1 is actually minted. We also pass _ into the token argument since we are working with an empty argument.

![56](../img/Lecture5/image9.png)

Next we will implement the policy and curSymbol functions. Each of these will only accept the oref since we are not working with the token names.

![57](../img/Lecture5/image8.png)

Finally, we need to implement the mint function. The mint function passes only the address here. Since the TokenName is a bytestring, we also need to declare it as:

![58](../img/Lecture5/image2.png)

We also only need to pass 2 arguments into the singleton function:

![59](../img/Lecture5/image39.png)

The mint function should then look like:

![60](../img/Lecture5/image37.png)

The final code should then look like:

![61](../img/Lecture5/image52.png)

The resulting output is:

![62](../img/Lecture5/image36.png)


![63](../img/Lecture5/image31.png)