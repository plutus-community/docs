# Lecture 9: Marlowe

Plutus Pioneer Program - Cohort 3 
March 17th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture9](https://youtube.com/playlist?list=PLNEK_Ejlx3x2zSFnzWA4Gbr_AVTz-4rzf)


## Table of Contents

- [Lecture 9: Marlowe](#lecture-9-marlowe)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 9](#preparation-for-lecture-9)
  - [Introduction](#introduction)
  - [Simon Thompson: Marlowe Overview](#simon-thompson-marlowe-overview)
  - [Alexander Nemash: Marlowe in Plutus](#alexander-nemash-marlowe-in-plutus)
  - [Brian Bush: The Marlowe CLI](#brian-bush-the-marlowe-cli)
  - [Marlowe Playground Demo](#marlowe-playground-demo)
  - [Homework](#homework)

## Preparation for Lecture 9

This week we will be learning how to use Marlowe in the browser. 

We can get started by visting Marlowe Playground here:
[https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/](https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/)

The Marlowe run demo can be found at:
[https://run.marlowe-finance.io/](https://run.marlowe-finance.io/)


We can now begin with the lecture.


## Introduction

Welcome to the second to last lecture of the Plutus Pioneer Program. This lecture is not 
about Plutus, but about Marlowe; which is a domain specific language for financial contracts built on top of Plutus. 

Marlowe has also changed a little bit since the recording of this lecture for the first iteration.
It was decided to not edit this lecture as much, because it is still mostly accurate and in particular the demo at the end still works.

In the previous lectures we have learnt about all the important ingredients for writing a Plutus application.

We have first looked at the extended UTxO model - the accounting model that Cardano uses - and the additions that Plutus brings to it.

Then we have talked about on-chain validation, minting policies, writing off-chain code, we have seen how to deploy smart contracts and also how to test them.

Plutus is a very powerful language. So powerful, in fact, that you can implement other languages on top of it - you can write an interpreter in Plutus for other languages.

One such language is Marlowe. Marlowe is a Domain Specific Language (DSL) for smart contracts.

For this lecture, Professor Simon Thompson, a very prominent figure in the Haskell community who leads the Marlowe team, and his colleague Alex Nemish will give guest lectures to tell us a bit about Marlowe.

In contrast  to previous iterations of this course it is now  actually possible to deploy Marlowe contracts on  the blockchain, on the Mainnet or on the Testnet.  
There is a very nice tool to do this, it is called the Marlowe-CLI, which is similar to  the Cardano-CLI. 
So in this lecture we will also  include a presentation by our colleague Brian bush, who will talk about Marlowe -CLI and explain how it works. 

Afterwards we will look at the Marlowe playground and play with a simple smart contract.

## Simon Thompson: Marlowe Overview

![Screenshot 2022-03-24 at 08-31-01 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159916706-2cac5426-b67c-4969-a9e1-e8e2d3474131.png)

![Screenshot 2022-03-24 at 08-31-52 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159916827-2dd81e8d-4f93-42cc-930e-100ecc9d8a5d.png)

Marlowe is a special-purpose language for writing financial contracts on Cardano.

### Why do we build DSLs?

One reason is that we want to build languages that are closer to the language of the user and not so much the language of the system. They are designed to be in the specific domain of the application. A financial language will talk about payments, for example.

When we write a DSL, we get some advantages. We can write down things in that domain, but we can't perhaps write as much as we could in a general purpose language. And, if we do work in this more specialised context, we have the advantage of being able to give people better feedback and better error messages. We can also give more guarantees on program behaviour. That's one of the things that will be stressed in this lecture.


![Screenshot 2022-03-24 at 08-34-11 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917160-4312cec9-813f-4087-b306-cd421baaeaa5.png)

### What kind of assurance can we give?

We can give two kinds of assurance. We can make sure that contracts do what they are supposed to do, but we can also make sure that they don't do what they shouldn't. We will see both aspects of that as we go along.

We've designed the language to be a simple as possible and the implementation reflects that, and we'll talk a bit about that later on. Contracts are nice and readable, and also we can easily simulate them, so we can present to users a very clear picture of how their contract in Marlowe will behave.

In fact, we can do more than that. Because they are particularly restricted, we can explore every possible behavior path that a contract can take, before it is executed. So, we can give complete guarantees about how a contract will behave, not just on one or two tests, but on every possibly execution sequence.

It's also more straightforward to write mathematical proofs of various kinds of safety, so that is the strongest criteria that we can hit in this kind of world; a mathematical proof that the system will do certain things and won't do others.


![Screenshot 2022-03-24 at 08-36-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917538-75816d4c-1744-4924-a834-87e98743cc33.png)

### What does a financial contract do?

Let's start by looking at what a financial contract can do.

![Screenshot 2022-03-24 at 08-38-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917887-2d50f403-379e-4d5c-862c-5db631bff116.png)

A contract can accept payments from participants in the contract.

![Screenshot 2022-03-24 at 08-39-13 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918025-0b5cd830-7144-462e-8a8f-9d127c2d981c.png)


Depending on choices made by one the participants, it can evolve in different directions.

![Screenshot 2022-03-24 at 08-39-47 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918096-9e096f69-06da-4c99-adef-bcb23ad03d11.png)

It can make decisions based on external information such as the information coming from a stock exchange. So, information coming from an oracle can determine the future behaviour of a contract.

![Screenshot 2022-03-24 at 08-40-37 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918218-2d123f1a-3138-46e1-9d08-da76ea976634.png)

A contract can also make payments out. If money has been deposited in the contract, that money can be deposited out to participants.

![Screenshot 2022-03-24 at 08-41-45 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918420-32d34fc0-6ccc-4a28-96df-2282c9c1eee8.png)

So we have flows of money and choices according to external factors.

One final thing that we have is that the roles in a contract are things that themselves can be owned. We represent that in Marlowe by minting tokens that represent those roles. That means that we can use those tokens as evidence that somebody is meant to be playing a role. They are a form of security that a person submitting a transaction is allowed to submit that transaction, but also it means that these roles are tradable. A role could be traded by another person or another contract.

![Screenshot 2022-03-24 at 08-44-30 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918885-d963f50b-4960-4d81-bb33-6771b14ccf09.png)

### Design

Now let's think about how to design a language based on these ingredients.

![Screenshot 2022-03-24 at 08-45-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159919037-98a45de5-15a6-4975-9290-c6ebd62728e3.png)

When we design a language of contracts, what we are really doing is designing a programming language. A smart contract is just a program running on a blockchain.

A contract could, in principle, run forever. And also, more subtly, it could get stuck waiting for an input forever.

It could terminate while holding assets, locking them up forever.

So there's a whole lot of security issues that a program might have.

![Screenshot 2022-03-24 at 08-48-13 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159919605-d43745ec-fbda-444e-b8d3-1e2b9b5acc34.png)


### Designed for safety

What we chose to do was to design for safety.
Contracts are finite

Firstly, contracts are designed to be finite. Their life will be finite, there is no recursion or looping in Marlowe. We will come back to that a bit later on when we talk about Marlowe being embedded in other languages.

### Contracts will terminate

We can be sure that contracts will terminate. We do that by putting timeouts on every external action. Every choice, every deposit of money into the contract comes with a deadline. Marlowe contracts cannot wait forever for somebody to make a choice or for an action to happen. If you hit the timeout then an alternative course is taken.

### No assets retained on close

We've designed the semantics of the language so that when a contract reaches its close, at the end of its lifetime, any money left in the contract will be refunded to participants.

### Conservation of value

Conservation of value is something that we get for free from the underlying blockchain. The blockchain guarantees that we can't double spend and because we are using the transaction mechanisms of the underlying blockchain, we can be sure that we are getting conservation of value.

So this is giving us a lot of guarantees out of the box. These are not guarantees that you get from Plutus contracts in general. A Plutus contract could go on forever, it need not terminate and it could terminate while holding a whole collection of assets which then become unreachable.


![Screenshot 2022-03-24 at 08-51-36 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159920158-29260325-e294-47a1-b6bc-8d9a03056ddd.png)

### The Marlowe Language

So what does the language look like? Let's cut to the chase.

Marlowe, at heart is represented as a Haskell datatype.

```haskell
data Contract = Close
| Pay Party Payee Value Contract
| If Observation Contract Contract
| When [Case Action Contract] Timeout Contract
| Let ValueId Value Contract
| Assert Observation Contract
deriving (Eq,Ord,Show,Read,Generic,Pretty)
```

We have a ```Pay``` construct. In that a Party in the contract makes a payment to a Payee of a particular Value, and then the contract continues with what we call the continuation contract.

```haskell
Pay Party Payee Value Contract
```

We can go in two separate directions. We can observe ```If``` a particular Observation is true or not. If the observation is true we follow the first Contract, if it is false we follow the second Contract.

```haskell
If Observation Contract Contract
```

The most complex construct in Marlowe is the ```When``` construct. It takes three arguments. The first of those is a list of Contract/Action pairs - a list of Cases.

```haskell
When [Case Action Contract] Timeout Contract
```

What the When construct does is wait for one of a number of Actions. When one of those Actions happens, it performs the corresponding Contract. For example, it could be waiting for a deposit. If we have a case where the first part of the pair is a deposit, then we execute the corresponding second part of the pair. Similarly with making a choice or with getting a value from an oracle.

Here we are waiting for external actions and, of course, the contract can\'t make those actions happen. A contract can\'t force somebody to make a choice. It can't force somebody to make a deposit. But what we can do is say that if none of these actions takes place then we will hit the Timeout, and when we hit the Timeout, we will perform the Contract represented by the final argument to the When construct.

So, we can guarantee that something will happen in the When construct, either by one of the actions triggering a successive contract, or we hit the timeout and go to that continuation.

Finally we have the ```Close``` construct which has the semantics defined so that nothing is retained when we close.

```haskell
data Contract = Close
```

That is the Marlowe language, and we will see that we can use these to construct Marlowe contracts in a variety of ways.


![Screenshot 2022-03-24 at 08-56-53 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159921044-8f6d48e3-21e9-40ba-9987-8bcc8870cc7d.png)

### The Marlowe Product

So that is the language. What is the Marlowe product itself?

We have a suite of things. First we'll look at the overall vision for Marlowe and then look at where we are in terms of fulfilling that vision.


![Screenshot 2022-03-24 at 09-57-42 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159932268-252fad81-3566-4275-b292-7f8546c12d41.png)

We have a prototype for Marlowe Run. That is the system through which an end user will interact with contracts running on the Cardano blockchain. You can think of Marlowe Run as the Marlowe DAPP. It's the things that allows Marlowe contracts to be executed.

We're also building a market where contracts can be uploaded, downloaded, and where we can provide various kinds of assurance about those contracts.

We allow contracts to be simulated interactively and we call that Marlowe Play. We allow contracts to be built in various different ways and we call that Marlowe Build. In fact fact what we've done at the moment is bundle those two - Marlowe Play and Build - into what we call the Marlowe Playground.

So as things stand at the moment you can use the Marlowe Playground to simulate and construct Marlowe contracts we're in the process of redesigning the user experience based on what we've done with Marlowe Run.

This is the prototype of how end users will interact with Marlowe on the blockchain. Our intention is that we'll have all these products available running on the Cardano blockchain when we have the full support for this which will involve having the Plutus Application Backend and the wallet back end and so on working as they should.



### Marlowe Run Demo

We'll now look at a demo of what we have in Marlowe Run to give you a sense of what we can do at the moment in terms of giving users the experience that they will have when Marlowe is running on blockchain. This will be the app that is going to provide that experience.

You can find the Marlowe Run demo at:

[https://run.marlowe-finance.io/](https://run.marlowe-finance.io/)


![Screenshot 2022-03-24 at 10-01-38 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159933110-83085082-c99f-477a-ac8d-3761afa59d58.png)


Marlowe run runs in the browser and what it does is provide the end user interaction with contracts running on the blockchain.

For the moment we're simulating that blockchain inside the browser but eventually this will be the tool you\'ll use to run contracts for real on Cardano.

To interact with the contract your wallet needs to be involved to control your your signature and to control your assets, so we link up Marlowe to run with a wallet. Let's link it up with Shruti's wallet. You can do this by creating a demo wallet, or by selecting an existing wallet.

### Create wallet for Shruti

![Screenshot 2022-03-24 at 10-03-39 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159933489-10f26cbe-2cfe-4524-9e43-fdcd3c5d2104.png)


![Screenshot 2022-03-24 at 10-05-10 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159933821-af5caa58-1d49-40ca-8c81-c2652b1d225c.png)

In this window we see the world from Shruti's perspective. Let's open up another window and link that window to the world from Charles's perspective.

### Create Wallet for Charles

![Screenshot 2022-03-24 at 10-06-24 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159934088-f3e032b3-a412-4c0c-ac5f-24d009ed1a22.png)


![Screenshot 2022-03-24 at 10-06-42 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159934146-be84e6fd-2d79-4fa9-aec6-502ef1bb3cd3.png)

At the moment neither of them has any contracts running. They have a blank space, but let's start a contract up. Let's set up a zero coupon bond which is a fancy name for a loan. 

Let's suppose that Charles is making a loan to Shruti. He's the investor she's the issuer of the bond.


![Screenshot 2022-03-24 at 10-08-04 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159934448-920d5612-3ef3-4edb-bb72-e99c9c7fd976.png)


![Screenshot 2022-03-24 at 10-10-37 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159934926-a9536ded-716a-48be-9d7d-a3ad90dfc6e5.png)

Shruti wants to borrow 10 Ada from Charles and she's promised to pay back 11 Ada. So we've said who the issuer and investor are we said what the price and the eventual value will be and we're now going to create the contract. In order to do that we have to make a payment of 10 lovelace to get the contract started.

![Screenshot 2022-03-24 at 10-12-15 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159935252-37457cb8-eb86-4dae-9884-a2117e252ca9.png)

So let's pay. We are asked to approve and the payment goes through. You can see now in Shruti's Marlowe Run we've got the Zero Coupon Bond running, but also, if you look at Charles's view of the world, it's running there too for him.


![Screenshot 2022-03-24 at 10-16-50 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936140-702d2ded-6398-481e-baef-d5d17065b47c.png)


![Screenshot 2022-03-24 at 10-17-11 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936205-17af04b5-9f80-45be-9792-ace9d1eb1d3e.png)


We're at the first step. If we click through on Shruti's contract, it's saying that it's waiting for something from the investor, who is Charles.


![Screenshot 2022-03-24 at 10-18-43 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936516-0ce84a59-23b1-4a33-b9c8-4ba24f95e894.png)



So let's see what's happening in his view.


![Screenshot 2022-03-24 at 10-18-24 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936469-0996165d-cd3b-47d5-a018-16367ae479c7.png)




He's being asked to make a deposit so let's click on that to make the deposit.



![Screenshot 2022-03-24 at 10-19-00 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936571-c6e7cd4e-2254-4602-b89b-f5aa72015dc5.png)



And click to confirm with a fee of 10 lovelace.

Then you can see his view has changed now he's waiting for the issuer to pay him back.

We look in Shruti's view, which is incidentally the mobile view, of Marlowe Run, and he's asked to pay her 1 Ada.


![Screenshot 2022-03-24 at 10-19-29 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936673-2715559d-50b4-4bf1-aacf-f369e6602824.png)



Let's make her do that now. He'll also have to pay a 10 lovelace transaction fee.



![Screenshot 2022-03-24 at 10-19-44 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936748-aa258360-99b8-4ca5-a065-3dddb1de0d3a.png)



Let's make that deposit.


And you see now from both their perspectives that loan is completed you can see the history of what's gone on. You can see, at particular points, the balances that the contract holds.

If we close that and select History, we can see the history of all the contracts that Shruti has taken part in.

![Screenshot 2022-03-24 at 10-20-37 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936897-dc709a9f-269d-4d1b-bce9-026af344f404.png)

![Screenshot 2022-03-24 at 10-20-47 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159936927-daad52da-d7d5-40ce-9d2f-69436f913f5b.png)


That pretty much covers the basics of what you get from Marlowe Run. It's an intuitive interface to a contract running on the blockchain. You see that each participant in the contract gets their view of the contract in real time, updated from what is, in this case in the browser, but eventually what's on the blockchain.

![Screenshot 2022-03-24 at 10-00-13 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159932867-bb12f7f0-6012-4d2f-8a93-6764bc960bc6.png)

### Engineering

Let's now take a look under the hood and see how Marlowe will be executed on Cardano.

Here's a diagram just to give you the context. You'll understand most parts of this diagram already. We a Cardano root node on which Plutus is running, and as you know, Plutus is a dialect of haskell, more or less.

![Screenshot 2022-03-24 at 10-27-21 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159938352-e8f4aaa2-5a4e-4acb-964d-ed760de87a9a.png)

Marlowe is embedded in Haskell and Marlowe is executed using Plutus. So Marlowe sits on top of Plutus, but it's also linked to Marlowe Run and has an attachment to a wallet you'll be able to interact with as an end user with a running Marlowe contract.

Also it gets linked to Oracles and so on sitting out there in the real world.

Now, what does it mean to to execute a Marlowe contract?

Again this will be familiar to you from Plutus but let's just talk through precisely how it works.

![Screenshot 2022-03-24 at 10-29-02 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159938722-a8c605ab-760a-41fa-8951-856ac2743c11.png)

Executing a Marlowe contract will produce a series of transactions on the blockchain. Obviously Plutus running on Cardano checks the validity of transactions. We have a validation function.

The validation function for these Marlowe transactions is essentially a Marlowe interpreter. It checks that the transactions indeed conform to the steps of the Marlowe contract. That's done using the EUTxO model, so we pass the current state of the contract and some other information through as datum.

The Marlowe interpreter uses that to ensure that the the transactions that are submitted meet the criteria for the particular Marlowe contract.

So that's the on chain part.

Obviously off chain there's a component as well. So we have to have Marlowe Run and we'll have to build the transactions that meet the the validation step on chain.

![Screenshot 2022-03-24 at 10-31-01 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159939173-8616f84e-5b71-4bc7-bc03-0efb57c8fd4d.png)

And, if and when the contract requires crypto assets it will have off chain code to ensure that transactions are appropriately signed so that we will have authorization for spending crypto assets.

Using Marlowe run and an associated wallet, we construct the transactions.

We get a flow of information in both directions. Marlowe run will submit transactions to the blockchain that then can be validated by the Marlowe interpreter, which is itself a Plutus contract. It's one of the largest Plutus contracts that exists.

![Screenshot 2022-03-24 at 10-34-45 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159939924-230b848d-c8c8-4868-8c77-1d42d197b591.png)

But there's also information flow another way because suppose that the transaction I've submitted is a deposit of money into a running contract, and suppose the contract also involves Charles Hoskinson, so my instance of Marlowe Run has submitted that transaction, but Charles also has to be notified about that.

The information flows in the other direction using the companion contract to ensure that every instance of Marlowe Run gets informed about activity in that contract.

Alex will talk some more about the details of the implementation but here you're seeing an outline of how it all how it all works.

Transactions are validated on chain through the interpreter, but they have to be built off chain and in some cases have to be authorized. Essentially the blockchain is the central synchronization point for the distributed system that is the collection of instances of Marlowe Run that are interacting to execute the contract

You saw in the demo that, in two separate windows, we were sharing information. That was simulating it locally but in production this will be information that's stored on the blockchain.

![Screenshot 2022-03-24 at 10-35-47 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159940103-deb3af4c-664e-4be6-b30e-1f5e76cba0ab.png)

### System Design

Let's talk a little bit about how the system is designed in in a high-level way.

Here's a piece of the semantics of Marlowe, and as you can see it's a Haskell function.

```haskell
-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of

    Close -> case refundOne (accounts state) of
        Just ((party, money), newAccounts) -> let
            newState = state { accounts = newAccounts }
            in Reduced ReduceNoWarning (ReduceWithPayment (Payment party (Party party) money)) newState Close
        Nothing -> NotReduced

    Pay accId payee tok val cont -> let
        amountToPay = evalValue env state val
        in  if amountToPay <= 0
            then let
                warning = ReduceNonPositivePay accId payee tok amountToPay
                in Reduced warning ReduceNoPayment state cont
            else let
                balance    = moneyInAccount accId tok (accounts state)
                paidAmount = min balance amountToPay
                newBalance = balance - paidAmount
                newAccs = updateMoneyInAccount accId tok newBalance (accounts state)
                warning = if paidAmount < amountToPay
                          then ReducePartialPay accId payee tok paidAmount amountToPay
                          else ReduceNoWarning
                (payment, finalAccs) = giveMoney accId payee tok paidAmount newAccs
                newState = state { accounts = finalAccs }
                in Reduced warning payment newState cont

    If obs cont1 cont2 -> let
        cont = if evalObservation env state obs then cont1 else cont2
        in Reduced ReduceNoWarning ReduceNoPayment state cont

    When _ timeout cont -> let
        startSlot = fst (timeInterval env)
        endSlot   = snd (timeInterval env)
        -- if timeout in future – do not reduce
        in if endSlot < timeout then NotReduced
        -- if timeout in the past – reduce to timeout continuation
        else if timeout <= startSlot then Reduced ReduceNoWarning ReduceNoPayment state cont
        -- if timeout in the time range – issue an ambiguity error
        else AmbiguousTimeIntervalReductionError

    Let valId val cont -> let
        evaluatedValue = evalValue env state val
        boundVals = boundValues state
        newState = state { boundValues = Map.insert valId evaluatedValue boundVals }
        warn = case Map.lookup valId boundVals of
              Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
              Nothing     -> ReduceNoWarning
        in Reduced warn ReduceNoPayment newState cont

    Assert obs cont -> let
        warning = if evalObservation env state obs
                  then ReduceNoWarning
                  else ReduceAssertionFailed
        in Reduced warning ReduceNoPayment state cont
```
We take an environment, the current state and a contract we executed, and based on what contract that is - a close perhaps, or a pay, we can reduce we can take some steps of computing the results of that contract.

We do that in a way that uses uses Haskell in a quite straightforward way to advance the contract. This specification in Haskell is an executable specification of the semantics and this gives us some very nice consequences.

![Screenshot 2022-03-24 at 10-43-45 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159941901-7197d209-c3d8-423d-a834-b66e270d49e3.png)

### Semantics 

We've got al we've got a high level description of what the semantics are, and we're doing that through something that is effectively an interpreter. So we're defining at a high level this interpreter in Haskell for Marlowe contracts.

### Completeness

One really nice thing about writing it in this sort of way is that we can be sure we cover all cases because it's a it will be obvious if we're missing some cases. Writing it as an interpreter ensures that we will hit cases we need to in describing the semantics.

### Engagement 

Also it really helps us to understand the semantics. When you're designing a language you have an abstract idea about what it's going to mean, but there's nothing like having a an implementation of it so you can actually run the semantics.

What would it mean if we were to add this construct? What would it mean if we were to modify the semantics in this way?

If we'd written it in a purely purely logical format, it's difficult to unscramble just from the rules as they're laid out what, precisely, a change in rule might mean.

What's even nicer is that we can reuse the semantics in a number of different ways.

![Screenshot 2022-03-24 at 10-44-34 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159942103-8b0f9e7c-c647-4b9f-9b75-a112647ec43b.png)

In the theorem prover Isabelle, we can use the semantics for reasoning and proof and we use pretty much the same semantics because Isabelle uses a functional language as is as its subject.

We can run the semantics in Plutus. Plutus is more or less Haskell, perhaps not with all the libraries, but we can, in principle at least, build our implementation on blockchain from our semantics, and also we can translate the semantics into PureScript for simulation in the browser.

![Screenshot 2022-03-24 at 10-46-18 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159942489-aacb9715-83bb-48a3-8339-695da4f0f872.png)

Now pure script is not the same exactly the same as Haskell. Isabelle's language is not exactly the same as Haskell. How can we be sure that all these versions are the same?

One way of doing it is to extract Haskell code from Isabelle and test the original against um this extracted code. We do that on random contracts and that gives us a pretty high level of assurance that the two are the same.

Down down the line in our road map we certainly expect to be using a Haskell and Javascript implementation at some point to replace PureScript in the front end so we don't have to write a PureScript version of the semantics when we're doing the off chain interpretation building the transactions to be submitted. We can use the real haskell implementation by compiling it into Javascript and running that in Marlowe Run in the client code.

So, building the language in Haskell means that though we use various different versions of the semantics, we can get a high level of assurance that these are the same and indeed we can in some situations replace things like the PureScript by Javascript.


![Screenshot 2022-03-24 at 10-48-07 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159942935-8c4ad2d4-2cb6-4526-a487-64672619023c.png)


### Usability

That gives us a picture about how how the system is put together. Let's go to another aspect of Marlowe. We we talked about it being a special purpose language, and that being a DSL promoted usability.

Let's say a bit more about that.

![Screenshot 2022-03-24 at 10-48-48 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159943081-ca283fc2-80c2-402e-a5c4-cfe2885b34c3.png)

One way we we promote usability is that we provide different ways of writing contracts. Another way we promote usability is to allow people to explore interactively how contracts behave before they\'re actually run in the simulation.

So let's talk about those now.

We want to write a Marlowe contract, how can we do it? Well, we can write Haskell using the Marlowe data type as text. That's one way we can do it and that's fine. We have an editor for that inside the playground that supports code completion and will make suggestions and and so on.

So we can build the contracts as pure Marlowe, but there are other routes as well.

![Screenshot 2022-03-24 at 10-51-13 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159943689-47ea8b39-c11d-476b-8dc5-3159c3c5c963.png)

We have a visual editor for Marlowe so that you can produce Marlowe contracts visually, putting together blocks in a way that doesn't require you to be a confident programmer. You can start off by using the visual version as a way of learning to engage with Marlowe if you are a coder.

![Screenshot 2022-03-24 at 10-55-16 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159944635-3e65edc3-8ca1-425d-a0fe-4a59cb2191f1.png)

Marlowe is embedded in Haskell and in Javascript so we can use facilities like recursion to describe Marlowe contracts. We can say, in Haskell, let's do this particular pattern of behavior a certain number of times. We can write that in Haskell and then for a particular contract we convert the Haskell into Marlowe, and we can also do that for Javascript.

![Screenshot 2022-03-24 at 10-56-40 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159944896-eecf121b-8a10-4e6e-b972-10a1fd3f78c3.png)

Finally, something we're not going to talk about anymore in this talk is that we can generate contracts from initial conditions. We've been looking at that for the actor standard of financial contracts. On the basis of the contract terms we generate code in Marlowe. We write functions whose output is Marlowe code.

We provide users with a variety of different approaches, leveraging knowledge of Javascript, for example, or leveraging a non-code-based approach for describing the contracts

![Screenshot 2022-03-24 at 11-17-18 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/159949375-974ac7d2-49f2-475e-a172-abd8856ae2d8.png)

We also allow people to simulate the behavior of contracts. This is something that you can see in the current version of the Marlowe Playground.

That's something you can play with yourselves. We are looking at different ways of describing the results of a simulation. So at the moment we have a transaction log. We are allowed to choose an next action to perform, you can undo the last step to take you back and then try another path so you can step interactively backwards and forwards through the source code through the application of the contract.

![Screenshot 2022-03-24 at 11-18-04 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159949600-9b0b1cbe-5200-43a3-8a58-1ea9e4f91111.png)

What we're looking at is changing the user interface Marlowe Playground so that we'll use something rather more like the Marlowe Run run description of a running contract.


![Screenshot 2022-03-24 at 11-18-48 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159949751-4ebf4212-886b-4ff2-927d-e35643656183.png)


### Assurance

We've talked about usability. What about the sort of assurance that Marlowe can give users?

![Screenshot 2022-03-24 at 11-19-35 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159949908-4e3bb631-cff6-4764-a982-6a130fe4d59e.png)

We've seen we've seen that making the system transparent, that making code readable is itself an advantage. We've seen that there's simulation to give people to validate their intuition about a contract.

But rather more formally we can use the power of logic to do two things for us. We can do what's called static analysis so we can automatically verify properties of individual contracts. That means we can guarantee this contract will behave as it should, checking every route through the contract.

Also we can do machine-supported proof so, not automatic any longer, written by a user, but we can prove properties of the overall system.


![Screenshot 2022-03-24 at 11-20-42 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159950114-bab50110-7cb4-4589-a7d1-1f8563657bbb.png)

### Static Analysis

What static analysis allows us to do is check all execution paths through a Marlowe contract. All choices, all choices of slots for a submission of a transaction so we examine every possible way in which the contract might be executed.

The canonical example here is the example of whether a pay construct might fail. Is it possible a pay construct could fail? The answer is that we will use what's called an SMT solver An SMT is an automatic logic tool - the one we use is called Z3, although others are available. The SMT solver effectively checks all execution parts.

If a property is is satisfied that's fine, we get get the result. If it's not satisfied, we get a counter example. We get told that there's a way through this contract that leads to a failed payment - a payment that can't be fulfilled. So it gives an example of how it can go wrong, and that's really helpful. It means that if you really want to make sure that a failed payment can't happen, then this gives you a mechanism to understand and to debug how that eventuality can happen, and so gives you a chance to think about how to avoid it.

So, very powerful and entirely push button. You push a button and you get the results


![Screenshot 2022-03-24 at 11-24-59 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159951048-f27f4619-e7e4-4780-9164-e44ff6ad37a1.png)

Here you see a fragment of a Marlowe contract. It's an escrow contract where the contract starts with a deposit of 450 lovelace.

Checking the analysis in the playground, we've got the results. Static analysis could not find any any execution that results in any warning, so that\'s saying that you're okay - it's not going to give you a warning whatever you do.

But if we change that deposit of 450 lovelace to a deposit of 40 and analyze we then get this warning.

We get a transaction partial payment. We're told that we get to a payment where we're meant to pay 450 units of lovelace but there are only 40 available, and we get given a list of transactions that take us there.

So we're able to see from that how we got to that point, and the problem is that we didn't put enough money in and then we reached a place where we needed to make a payment of 450 lovelace.

So it's easy for us to see that we need to either make the payment smaller or make the initial deposit bigger. As it's entirely push button, we get that sort of assurance for free, as it were.

![Screenshot 2022-03-24 at 11-27-43 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159951644-9bf04d06-59fc-4818-b7d9-5373ba2081e5.png)

But thinking about verification, we can do rather more than that. We can prove properties of the system once and for all.

So, for example, we can prove from the semantics that accounts inside a Marlowe contract never go negative. You can't ever overdraw an account in a Marlowe contract.

We can also prove this theorem of money preservation. We can prove that if we look at all the money that's gone into the contract so far, that's equal to the sum of two things - the amount of money that's held inside the contract plus the amount of money that has been paid out. That gives a clear picture of money preservation.

We're also able to to prove other more technical things about the system. For example, that a Close construct will never produce any warnings. So, if we're analyzing for warnings, we don't need to worry about Close constructs. That allows us to optimize the static analysis.

We're also able to prove that the static analysis, which makes a number of simplifications to speed things up, is sound and complete. That means the static analysis will give us an error warning when the real contract can generate an error warning and it won't give us an error warning if the real contract can't do that.

One thing that we haven't done but is on our road map is to do these sorts of proofs for individual contracts or individual contract templates. Things that we can't necessarily prove with static analysis, we can prove by proving them by hand.

The system is amenable to having these proofs written about it, and they give us the highest level of assurance about how it works.

We've said enough for the moment about Marlowe. Where can you go to find out more?

![Screenshot 2022-03-24 at 11-30-11 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159952166-43a82cdd-7a02-4418-8435-29f2c4a0bee0.png)

There's a Marlowe GitHub repository that has the semantics and the basics about Marlowe.

[https://github.com/input-output-hk/marlowe](https://github.com/input-output-hk/marlowe)

Quite a lot of the implementation of the tools from Marlowe is in the Plutus repository because it has that repository as a dependency.

If you look in the IOHK online research library and search for Marlowe you'll find a number of research papers we've written about how the system works.

You'll also find an online tutorial in the Marlowe Playground.

[https://marlowe-playground-staging.plutus.aws.iohkdev.io/doc/marlowe/tutorials/index.html](https://marlowe-playground-staging.plutus.aws.iohkdev.io/doc/marlowe/tutorials/index.html)

Finally, Alex is going to give some more information in his presentation coming up next.

### Summary 

Just to summarize, what we have in Marlowe is a DSL, a special-purpose language for financial contracts, running on top of Plutus. Because it's a DSL it allows us to give assurance that is harder to give for a general purpose language. And we get assurance of they way contracts should and shouldn\'t behave.

It also allows us to to orient its design around users as well as developers. The language is simple and therefore we get readability.

We also get simulatability and we get these stronger assurances of static analysis and verification.


## Alexander Nemash: Marlowe in Plutus

The files for Marlowe have been updated since the video was recorded, therefore these lecture notes are modified for the respective changes. The files of interest can be found at:

[https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe/src/Language/Marlowe](https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe/src/Language/Marlowe)

Alex Nemish is one of the Marlowe developers and in this presentation, he shows us a bit of Marlowe semantics and Marlowe PAB (Plutus Application Backend) contracts.

We'll start with a brief description of Marlowe Semantics. Then we'll look at the PAB contracts.

Here are the main data types for Marlowe.



```haskell

data Contract = Close
              | Pay AccountId Payee Token (Value Observation) Contract
              | If Observation Contract Contract
              | When [Case Contract] Timeout Contract
              | Let ValueId (Value Observation) Contract
              | Assert Observation Contract
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (Pretty)
```
It's a contract. Essentially those are six constructors that you can start to model a contract with and here's the state that is going to be stored on a blockchain.

```haskell
data State = State { accounts    :: Accounts
                   , choices     :: Map ChoiceId ChosenNum
                   , boundValues :: Map ValueId Integer
                   , minTime     :: POSIXTime }
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
```
So we have a state of balances of accounts by party, we have a map of choices, we have bound values which come from the Let constructor, and a minTime which is the POSIXTime that the contract sees.

```haskell
data InputContent = IDeposit AccountId Party Token Integer
                  | IChoice ChoiceId ChosenNum
                  | INotify
  deriving stock (Haskell.Show,Haskell.Eq,Generic)
  deriving anyclass (Pretty)
```
The ```InputContent``` data type essentially contains actions for a Marlowe contract. It is either a deposit, a choice, or a notification.

```haskell
data TransactionInput = TransactionInput
    { txInterval :: TimeInterval
    , txInputs   :: [Input] }
  deriving stock (Haskell.Show, Haskell.Eq)
```
Here is the TransactionInput datatype. This is what we give as an input. Every transaction has a defined time interval and a list of inputs

```haskell
data TransactionOutput =
    TransactionOutput
        { txOutWarnings :: [TransactionWarning]
        , txOutPayments :: [Payment]
        , txOutState    :: State
        , txOutContract :: Contract }
    | Error TransactionError
  deriving stock (Haskell.Show)
```
And we have ```TransactionOutput``` which contains the payments that we expect to happen, the output state and the output contract.

```haskell
data MarloweData = MarloweData {
        marloweState    :: State,
        marloweContract :: Contract
    } deriving stock (Haskell.Show, Haskell.Eq, Generic)
      deriving anyclass (ToJSON, FromJSON)
```

We also see MarloweData which is essentially what is going to be stored on the blockchain. It's the current state of a contract as well as the actual contract.

```haskell
-- | Try to compute outputs of a transaction given its inputs, a contract, and it's @State@
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract = let
    inputs = txInputs tx
    in case fixInterval (txInterval tx) state of
        IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
            ApplyAllSuccess reduced warnings payments newState cont ->
                    if not reduced && (not (isClose contract) || (Map.null $ accounts state))
                    then Error TEUselessTransaction
                    else TransactionOutput { txOutWarnings = warnings
                                           , txOutPayments = payments
                                           , txOutState = newState
                                           , txOutContract = cont }
            ApplyAllNoMatchError -> Error TEApplyNoMatchError
            ApplyAllAmbiguousTimeIntervalError -> Error TEAmbiguousTimeIntervalError
            ApplyAllHashMismatch -> Error TEHashMismatch
        IntervalError error -> Error (TEIntervalError error)
```
The entrance to the semantics is the ```computeTransaction``` function. It gets the transaction input, the current state and the current contract and returns the transaction output.

First of all we check the time interval for errors. For example, we do not allow the time interval to contain any timeouts.

Then we apply all inputs and if this is successful we return the transaction output with any warnings we have found, the payments we expect, the new state and the continuation contract.

```haskell
-- | Apply a list of Inputs to the contract
applyAllInputs :: Environment -> State -> Contract -> [Input] -> ApplyAllResult
applyAllInputs env state contract inputs = let
    applyAllLoop
        :: Bool
        -> Environment
        -> State
        -> Contract
        -> [Input]
        -> [TransactionWarning]
        -> [Payment]
        -> ApplyAllResult
    applyAllLoop contractChanged env state contract inputs warnings payments =
        case reduceContractUntilQuiescent env state contract of
            RRAmbiguousTimeIntervalError -> ApplyAllAmbiguousTimeIntervalError
            ContractQuiescent reduced reduceWarns pays curState cont -> case inputs of
                [] -> ApplyAllSuccess
                    (contractChanged || reduced)
                    (warnings ++ convertReduceWarnings reduceWarns)
                    (payments ++ pays)
                    curState
                    cont
                (input : rest) -> case applyInput env curState input cont of
                    Applied applyWarn newState cont ->
                        applyAllLoop
                            True
                            env
                            newState
                            cont
                            rest
                            (warnings
                                ++ convertReduceWarnings reduceWarns
                                ++ convertApplyWarning applyWarn)
                            (payments ++ pays)
                    ApplyNoMatchError -> ApplyAllNoMatchError
                    ApplyHashMismatch -> ApplyAllHashMismatch
    in applyAllLoop False env state contract inputs [] []
  where
    convertApplyWarning :: ApplyWarning -> [TransactionWarning]
    convertApplyWarning warn =
        case warn of
            ApplyNoWarning -> []
            ApplyNonPositiveDeposit party accId tok amount ->
                [TransactionNonPositiveDeposit party accId tok amount]

```

So what happens in ```applyAllInputs```?

First of all, it's a loop. It uses the ```reduceContractUntilQuiescent``` function which reduces the contract until it reaches a quiescent state. Once we reach a quiescent state, we take the first input and try to apply it, and then continue with the loop, until we get an empty input list. Then we return the current state and the continuation contract.

The ```reduceContractUntilQuiescent``` function goes through a loop and tries to apply ```reduceContractStep``` which essentially evaluates a contract.

```haskell

-- | Reduce a contract until it cannot be reduced more
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent env state contract = let
    reductionLoop
      :: Bool -> Environment -> State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop reduced env state contract warnings payments =
        case reduceContractStep env state contract of
            Reduced warning effect newState cont -> let
                newWarnings = case warning of
                                ReduceNoWarning -> warnings
                                _               -> warning : warnings
                newPayments  = case effect of
                    ReduceWithPayment payment -> payment : payments
                    ReduceNoPayment           -> payments
                in reductionLoop True env newState cont newWarnings newPayments
            AmbiguousTimeIntervalReductionError -> RRAmbiguousTimeIntervalError
            -- this is the last invocation of reductionLoop, so we can reverse lists
            NotReduced -> ContractQuiescent reduced (reverse warnings) (reverse payments) state contract

    in reductionLoop False env state contract [] []
```

```haskell
-- | Carry a step of the contract with no inputs
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of

    Close -> case refundOne (accounts state) of
        Just ((party, money), newAccounts) -> let
            newState = state { accounts = newAccounts }
            in Reduced ReduceNoWarning (ReduceWithPayment (Payment party (Party party) money)) newState Close
        Nothing -> NotReduced

    Pay accId payee tok val cont -> let
        amountToPay = evalValue env state val
        in  if amountToPay <= 0
            then let
                warning = ReduceNonPositivePay accId payee tok amountToPay
                in Reduced warning ReduceNoPayment state cont
            else let
                balance    = moneyInAccount accId tok (accounts state)
                paidAmount = min balance amountToPay
                newBalance = balance - paidAmount
                newAccs = updateMoneyInAccount accId tok newBalance (accounts state)
                warning = if paidAmount < amountToPay
                          then ReducePartialPay accId payee tok paidAmount amountToPay
                          else ReduceNoWarning
                (payment, finalAccs) = giveMoney accId payee tok paidAmount newAccs
                newState = state { accounts = finalAccs }
                in Reduced warning payment newState cont

    If obs cont1 cont2 -> let
        cont = if evalObservation env state obs then cont1 else cont2
        in Reduced ReduceNoWarning ReduceNoPayment state cont

    When _ timeout cont -> let
        startSlot = fst (timeInterval env)
        endSlot   = snd (timeInterval env)
        -- if timeout in future – do not reduce
        in if endSlot < timeout then NotReduced
        -- if timeout in the past – reduce to timeout continuation
        else if timeout <= startSlot then Reduced ReduceNoWarning ReduceNoPayment state cont
        -- if timeout in the time range – issue an ambiguity error
        else AmbiguousTimeIntervalReductionError

    Let valId val cont -> let
        evaluatedValue = evalValue env state val
        boundVals = boundValues state
        newState = state { boundValues = Map.insert valId evaluatedValue boundVals }
        warn = case Map.lookup valId boundVals of
              Just oldVal -> ReduceShadowing valId oldVal evaluatedValue
              Nothing     -> ReduceNoWarning
        in Reduced warn ReduceNoPayment newState cont

    Assert obs cont -> let
        warning = if evalObservation env state obs
                  then ReduceNoWarning
                  else ReduceAssertionFailed
        in Reduced warning ReduceNoPayment state cont
```

If we get a Close then we are in a quiescent state. If we get a payment, then we evaluate it, update the balances and then return the reduced contract.

We do the same for If, Let and Assert. But for When, we only evaluate it if it's timed out, otherwise we say that it's not reduced, and that the contract is quiescent.

In a nutshell, Marlowe contract evaluation consists of two steps.

    We reduce the contract until it is quiescent - it's either closed or we get to a When that's not timed out yet.
    We try to apply inputs and evaluate the contract further.

Let's see how it works from the client side.

As you may have noticed, the Marlowe semantics code is quite abstract and it doesn't depend on the Cardano system's actions. So let's take a look at the actual Marlowe validator that's being executed on-chain.

```haskell
smallTypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallTypedValidator = Scripts.mkTypedValidatorParam @TypedMarloweValidator
    $$(PlutusTx.compile [|| smallMarloweValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator
```
Here's the ```smallTypedValidator``` which calls the ```smallMarloweValidator`` code.

```haskell
smallMarloweValidator
    :: MarloweParams
    -> MarloweData
    -> MarloweInput
    -> ScriptContext
    -> Bool
smallMarloweValidator MarloweParams{rolesCurrency, rolePayoutValidatorHash}
    MarloweData{..}
    marloweTxInputs
    ctx@ScriptContext{scriptContextTxInfo} = do
    
    let ownInput = case findOwnInput ctx of
            Just i -> i
            _      -> traceError "I0" {-"Can't find validation input"-}
    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
            case txInfoValidRange scriptContextTxInfo of
                -- FIXME: Recheck this, but it appears that any inclusiveness can appear at either bound when milliseconds
                --        of POSIX time is converted from slot number.
                Interval.Interval (Interval.LowerBound (Interval.Finite l) _) (Interval.UpperBound (Interval.Finite h) _) -> (l, h)
                _ -> traceError "R0"
    let positiveBalances = traceIfFalse "B0" $ validateBalances marloweState

    {- Find Contract continuation in TxInfo datums by hash or fail with error -}
    let inputs = fmap marloweTxInputToInput marloweTxInputs
    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsOk = validateInputs inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = traceIfFalse "B1" $ inputBalance == scriptInValue
    
    let preconditionsOk = positiveBalances && balancesOk
```
This function is the meat of the validator.

It takes ```MarloweParams``` - which we'll talk about later, it takes ```MarloweData```, followed by ```MarloweInput``` which is essentially transaction input expressed in Cardano types. It will then return a boolean

We check that the balances are valid - we require balances to be positive.

We do not check that a transaction contains exact input payments. We only require an evidence from a party, e.g. a signature for PubKey party, or a spend of a 'party role' token. This gives huge flexibility by allowing parties to provide multiple inputs (either other contracts or P2PKH). Then, we check scriptOutput to be correct.

```haskell
    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    -- let computedResult = TransactionOutput [] [] (emptyState minSlot) Close
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

                payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints payoutsByParty
                checkContinuation = case txOutContract of
                    Close -> True
                    _ -> let
                        totalIncome = foldMap (collectDeposits . getInputContent) inputs
                        totalPayouts = foldMap snd payoutsByParty
                        finalBalance = inputBalance + totalIncome - totalPayouts
                        outConstrs = ScriptOutputConstraint
                                    { ocDatum = marloweData
                                    , ocValue = finalBalance
                                    }
                        in checkOwnOutputConstraint ctx outConstrs
            preconditionsOk && inputsOk && payoutsOk && checkContinuation
        Error TEAmbiguousTimeIntervalError -> traceError "E1"
        Error TEApplyNoMatchError -> traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "E4"
        Error TEUselessTransaction -> traceError "E5"
        Error TEHashMismatch -> traceError "E6"
```

We construct a ```TransactionInput``` given the time interval and list of inputs, and we call the ```computeTransaction``` function that we saw in semantics.hs.

With the computed result we construct a ```MarloweData``` with a new contract continuation and updated state.

We calculate the new total balance given the income and total income

```haskell

  where
    checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                    txOutValue == value && hsh == Just svh && txOutAddress == addr
    checkScriptOutput _ _ _ _ = False

    allOutputs :: [TxOut]
    allOutputs = txInfoOutputs scriptContextTxInfo

    marloweTxInputToInput :: MarloweTxInput -> Input
    marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
            Just (Datum d) -> let
                continuation = PlutusTx.unsafeFromBuiltinData d
                in MerkleizedInput input hash continuation
            Nothing -> traceError "H"
    marloweTxInputToInput (Input input) = NormalInput input

    validateInputs :: [Input] -> Bool
    validateInputs inputs = all (validateInputWitness . getInputContent) inputs
      where
        validateInputWitness :: InputContent -> Bool
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> True
          where
            validatePartyWitness (PK pk)     = traceIfFalse "S" $ scriptContextTxInfo `txSignedBy` pk
            validatePartyWitness (Role role) = traceIfFalse "T" -- "Spent value not OK"
                                               $ Val.singleton rolesCurrency role 1 `Val.leq` valueSpent scriptContextTxInfo

    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> Bool
    payoutConstraints payoutsByParty = all payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> traceIfFalse "P" $ value `Val.leq` valuePaidTo scriptContextTxInfo pk
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                hsh = findDatumHash dataValue scriptContextTxInfo
                addr = Ledger.scriptHashAddress rolePayoutValidatorHash
                in traceIfFalse "R" $ any (checkScriptOutput addr hsh value) allOutputs
```


To validate inputs, we check that the required signatures and role tokens are present.

Payments to parties go either to a public key ```PK pk```, or go to the validator ```Role role```, which simply checks, given a currency, that a transaction spends a role token.

For off-chain execution, we provide three Marlowe PAB contracts.

    Marlowe Follower Contract
    Marlowe Control Contract
    Marlowe Companion Contract

### Marlowe Follower Contract

```haskell
marloweFollowContract :: Contract FollowerContractState MarloweFollowSchema MarloweError ()
marloweFollowContract = awaitPromise $ endpoint @"follow" $ \params ->
  do
    let typedValidator = mkMarloweTypedValidator params
    marloweHistory params
      >>= maybe (pure InProgress) (updateHistory params)
      >>= checkpointLoop (follow typedValidator params)

  where
    follow typedValidator params = \case
        Finished -> do
            logInfo $ "MarloweFollower found finished contract with " <> show params <> "."
            pure $ Right InProgress -- do not close the contract so we can see it in Marlowe Run history
        InProgress -> do
            result <- waitForUpdateTimeout typedValidator never >>= awaitPromise
            case result of
                Timeout t -> absurd t
                Transition Closed{..} -> do
                    logInfo $ "MarloweFollower found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
                    tell @FollowerContractState (transition params historyInput)
                    pure (Right Finished)
                Transition InputApplied{..} -> do
                    logInfo $ "MarloweFollower found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
                    tell @FollowerContractState (transition params historyInput)
                    pure (Right InProgress)
                Transition Created{..} -> do
                    logInfo $ "MarloweFollower found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
                    tell @FollowerContractState (created params historyData)
                    pure (Right InProgress)

    updateHistory :: MarloweParams
                  -> History
                  -> Contract FollowerContractState MarloweFollowSchema MarloweError ContractProgress
    updateHistory params Created{..} =
      do
        logInfo $ "MarloweFollower found contract created with " <> show historyData <> " by " <> show historyTxOutRef <> "."
        tell $ created params historyData
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory params InputApplied{..} =
      do
        logInfo $ "MarloweFollower found contract transitioned with " <> show historyInput <> " by " <> show historyTxOutRef <> "."
        tell $ transition params historyInput
        maybe (pure InProgress) (updateHistory params) historyNext
    updateHistory params Closed{..} =
      do
        logInfo $ "MarloweFollower found contract closed with " <> show historyInput <> " by TxId " <> show historyTxId <> "."
        tell $ transition params historyInput
        pure Finished
```
This is a very simple one - it contains only one endpoint called follow. It subscribes to all changes to a Marlowe contract validator address, so that we can store all the inputs that are applied to a Marlowe contract.

It uses the ```updateHistory``` function which, in a nutshell, finds a Marlowe input and constructs a TransactionInput data type, and uses tell to update the PAB contract state.

If you were connected to a web socket for this contract, you would be notified about transition changes.

The state of the contract is stored in ```ContractHistory```, which stores an initial MarloweParams, an initial MarloweData and a list of all TransactionInputs that were applied to this contract, and the script address. You can always restore the current state by applying a list of inputs to an initial state.

```haskell

data ContractHistory =
    ContractHistory
        { chParams      :: MarloweParams      -- ^ The "instance id" of the contract
        , chInitialData :: MarloweData        -- ^ The initial Contract + State
        , chHistory     :: [TransactionInput] -- ^ All the transaction that affected the contract.
                                              --   The current state and intermediate states can
                                              --   be recalculated by using computeTransaction
                                              --   of each TransactionInput to the initial state
        , chAddress     :: Address            -- ^ The script address of the marlowe contract
        }
        deriving stock (Show, Generic)
        deriving anyclass (FromJSON, ToJSON)
```

### Control Contract

The ```marlowePlutusContract``` is a control contract. It allows you to create an instance of a Marlowe contract, apply inputs to the instance, to auto-execute the contract, if possible, to redeem tokens from payments to roles, and to close the contract.

```haskell
{-  This is a control contract.
    It allows to create a contract, apply inputs, auto-execute a contract,
    redeem role payouts, and close.
 -}
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = selectList [create, apply, applyNonmerkleized, auto, redeem, close]
```

Let's go through Marlowe contract creation.

### Create Endpoint

```haskell
 create = endpoint @"create" $ \(reqId, owners, contract) -> catchError reqId "create" $ do
        -- Create a transaction with the role tokens and pay them to the contract creator
        -- See Note [The contract is not ready]
        ownPubKey <- unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] ownPubKey = " <> show ownPubKey
        let roles = extractNonMerkleizedContractRoles contract
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] roles = " <> show roles
        (params, distributeRoleTokens, lkps) <- setupMarloweParams owners roles
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] params = " <> show params
        time <- currentTime
        logInfo $ "Marlowe contract created with parameters: " <> show params <> " at " <> show time
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = State
                    { accounts = AssocMap.singleton (PK ownPubKey, Token adaSymbol adaToken) minLovelaceDeposit
                    , choices  = AssocMap.empty
                    , boundValues = AssocMap.empty
                    , minTime = time } }
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] marloweData = " <> show marloweData
        let minAdaTxOut = lovelaceValueOf minLovelaceDeposit
        let typedValidator = mkMarloweTypedValidator params
        let tx = mustPayToTheScript marloweData minAdaTxOut <> distributeRoleTokens
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] tx = " <> show tx
        let lookups = Constraints.typedValidatorLookups typedValidator <> lkps
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] lookups = " <> show lookups
        -- Create the Marlowe contract and pay the role tokens to the owners
        utx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx lookups tx)
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:create] utx = " <> show utx
        submitTxConfirmed utx
        logInfo $ "MarloweApp contract creation confirmed for parameters " <> show params <> "."
        tell $ Just $ EndpointSuccess reqId $ CreateResponse params
        marlowePlutusContract
```

When you call the create endpoint, you provide a contract and a map of roles to public keys. We then setup a ```MarloweParams```.

```haskell
data MarloweParams = MarloweParams {
        rolePayoutValidatorHash :: ValidatorHash,
        rolesCurrency           :: CurrencySymbol
    }
  deriving stock (Haskell.Show,Generic,Haskell.Eq,Haskell.Ord)
  deriving anyclass (FromJSON,ToJSON)
```
```MarloweParams``` is a way to parameterise a Marlowe contract. You can specify your own role payout validator by providing its hash. There is a default one that checks that the role token is spent within the transaction but you can do whatever you like.

When your contract uses roles, we need to know the currency symbol for the role. When the contract uses roles, we need to create role tokens and distribute them to their owners.

In the ```setupMarloweParams``` function we get the roles that are used within the contract. If we have owners for these roles, we create tokens with role names. By default we create one token per role. Then, in the same transaction, we distribute the role tokens to their owners.

```haskell
setupMarloweParams
    :: forall s e i o a.
    (AsMarloweError e)
    => RoleOwners
    -> Set Val.TokenName
    -> Contract MarloweContractState s e
        (MarloweParams, TxConstraints i o, ScriptLookups a)
setupMarloweParams owners roles = mapError (review _MarloweError) $ do
    if Set.null roles
    then do
        let params = marloweParams adaSymbol
        pure (params, mempty, mempty)
    else if roles `Set.isSubsetOf` Set.fromList (AssocMap.keys owners)
    then do
        let tokens = (, 1) <$> Set.toList roles
        txOutRef@(Ledger.TxOutRef h i) <- getUnspentOutput
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:setupMarloweParams] txOutRef = " <> show txOutRef
        txOut <-
          maybe
            (throwing _ContractError . Contract.OtherContractError . T.pack $ show txOutRef <> " was not found on the chain index. Please verify that plutus-chain-index is 100% synced.")
            pure
            =<< txOutFromRef txOutRef
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:setupMarloweParams] txOut = " <> show txOut
        let utxo = Map.singleton txOutRef txOut
        let theCurrency = Currency.OneShotCurrency
                { curRefTransactionOutput = (h, i)
                , curAmounts              = AssocMap.fromList tokens
                }
            curVali     = Currency.curPolicy theCurrency
            lookups     = Constraints.mintingPolicy curVali
                            <> Constraints.unspentOutputs utxo
            mintTx      = Constraints.mustSpendPubKeyOutput txOutRef
                            <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
        let rolesSymbol = Ledger.scriptCurrencySymbol curVali
        let minAdaTxOut = adaValueOf 2
        let giveToParty (role, addr) =
              mustPayToShelleyAddress addr (Val.singleton rolesSymbol role 1 <> minAdaTxOut)
        distributeRoleTokens <- foldMapM giveToParty $ AssocMap.toList owners
        let params = marloweParams rolesSymbol
        pure (params, mintTx <> distributeRoleTokens, lookups)
    else do
        let missingRoles = roles `Set.difference` Set.fromList (AssocMap.keys owners)
        let message = T.pack $ "You didn't specify owners of these roles: " <> show missingRoles
        throwing _ContractError $ Contract.OtherContractError message
```

### Apply Endpoint

```haskell

  apply = endpoint @"apply-inputs" $ \(reqId, params, timeInterval, inputs) -> catchError reqId "apply-inputs" $ do
        let typedValidator = mkMarloweTypedValidator params
        _ <- applyInputs params typedValidator timeInterval inputs
        tell $ Just $ EndpointSuccess reqId ApplyInputsResponse
        logInfo $ "MarloweApp contract input-application confirmed for inputs " <> show inputs <> "."
        marlowePlutusContract
```

The apply endpoint is very simple. We call the ```applyInputs``` function.

```haskell
applyInputs :: AsMarloweError e
    => MarloweParams
    -> SmallTypedValidator
    -> Maybe TimeInterval
    -> [MarloweClientInput]
    -> Contract MarloweContractState MarloweSchema e MarloweData
applyInputs params typedValidator timeInterval inputs = mapError (review _MarloweError) $ do
    -- TODO: Move to debug log.
    logInfo $ "[DEBUG:applyInputs] params = " <> show params
    logInfo $ "[DEBUG:applyInputs] timeInterval = " <> show timeInterval
    timeRange <- case timeInterval of
            Just si -> pure si
            Nothing -> do
                time <- currentTime
                pure (time, time + defaultTxValidationRange)
    logInfo $ "[DEBUG:applyInputs] timeRange = " <> show timeRange
    mkStep params typedValidator timeRange inputs
```


We construct a time range and we use the ```mkStep``` function which takes a params, typedValidator, timeRange  and a list of inputs.

### Redeem Endpoint

```haskell
   redeem = promiseMap (mapError (review _MarloweError)) $ endpoint @"redeem" $ \(reqId, MarloweParams{rolesCurrency}, role, paymentAddress) -> catchError reqId "redeem" $ do
        -- TODO: Move to debug log.
        logInfo $ "[DEBUG:redeem] rolesCurrency = " <> show rolesCurrency
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        logInfo $ "[DEBUG:redeem] address = " <> show address
        utxos <- utxosAt address
        let
          spendable txout =
            let
              expectedDatumHash = datumHash (Datum $ PlutusTx.toBuiltinData role)
              dh = either id Ledger.datumHash <$> preview Ledger.ciTxOutDatum txout
            in
              dh == Just expectedDatumHash
          utxosToSpend = Map.filter spendable utxos
          spendPayoutConstraints tx ref txout =
            do
              let amount = view Ledger.ciTxOutValue txout
              previousConstraints <- tx
              payOwner <- mustPayToShelleyAddress paymentAddress amount
              pure
                $ previousConstraints
                <> payOwner -- pay to a token owner
                <> Constraints.mustSpendScriptOutput ref unitRedeemer -- spend the rolePayoutScript address

        spendPayouts <- Map.foldlWithKey spendPayoutConstraints (pure mempty) utxosToSpend
        if spendPayouts == mempty
        then do
            logInfo $ "MarloweApp contract redemption empty for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse
        else do
            let
              constraints = spendPayouts
                  -- must spend a role token for authorization
                  <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
              -- lookup for payout validator and role payouts
              validator = rolePayoutScript rolesCurrency
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:redeem] constraints = " <> show constraints
            ownAddressLookups <- ownShelleyAddress paymentAddress
            let
              lookups = Constraints.otherScript validator
                  <> Constraints.unspentOutputs utxosToSpend
                  <> ownAddressLookups
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:redeem] lookups = " <> show lookups
            tx <- either (throwing _ConstraintResolutionContractError) pure (Constraints.mkTx @Void lookups constraints)
            -- TODO: Move to debug log.
            logInfo $ "[DEBUG:redeem] tx = " <> show tx
            _ <- submitTxConfirmed $ Constraints.adjustUnbalancedTx tx
            logInfo $ "MarloweApp contract redemption confirmed for role " <> show role <> "."
            tell $ Just $ EndpointSuccess reqId RedeemResponse

        marlowePlutusContract
```

The redeem endpoint allows you to get money that has been paid to a role payout script.

We get the address of the script and then send all the outputs to the token owner.


### Auto Endpoint

```haskell
 auto = endpoint @"auto" $ \(reqId, params, party, untilTime) -> catchError reqId "auto" $ do
        let typedValidator = mkMarloweTypedValidator params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                then autoExecuteContract reqId params typedValidator party md
                else do
                    tell $ Just $ EndpointSuccess reqId AutoResponse
                    marlowePlutusContract

        maybeState <- getOnChainState typedValidator
        case maybeState of
            Nothing -> do
                wr <- waitForUpdateUntilTime typedValidator untilTime
                case wr of
                    Transition Closed{} -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Transition InputApplied{historyData} -> continueWith historyData
                    Transition Created{historyData} -> continueWith historyData
            Just (OnChainState{ocsTxOutRef=st}, _) -> do
                let marloweData = toMarloweState st
                continueWith marloweData
    -- The MarloweApp contract is closed implicitly by not returning
    -- itself (marlowePlutusContract) as a continuation
```
The auto endpoint is quite interesting and quite complicated. There is a set of contracts that can be executed automatically.

Imagine a contract that contains only deposits and payouts. No participant needs to provide choices or any interactive stuff. There are only scheduled payments. Such a contract can be executed automatically, and the auto endpoint allows exactly that.

So, if a contract can be executed automatically, it calls ```autoExecuteContract```.

```haskell

autoExecuteContract :: UUID
                      -> MarloweParams
                      -> SmallTypedValidator
                      -> Party
                      -> MarloweData
                      -> Contract MarloweContractState MarloweSchema MarloweError ()
    autoExecuteContract reqId params typedValidator party marloweData = do
        time <- currentTime
        let timeRange = (time, time + defaultTxValidationRange)
        let action = getAction timeRange party marloweData
        case action of
            PayDeposit acc p token amount -> do
                logInfo @String $ "PayDeposit " <> show amount <> " at within time " <> show timeRange
                let payDeposit = do
                        marloweData <- mkStep params typedValidator timeRange [ClientInput $ IDeposit acc p token amount]
                        continueWith marloweData
                catching _MarloweError payDeposit $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry PayDeposit in 2 seconds"
                    _ <- awaitTime (time + 2000)
                    continueWith marloweData
            WaitForTimeout timeout -> do
                logInfo @String $ "WaitForTimeout " <> show timeout
                _ <- awaitTime timeout
                continueWith marloweData
            WaitOtherActionUntil timeout -> do
                logInfo @String $ "WaitOtherActionUntil " <> show timeout
                wr <- waitForUpdateUntilTime typedValidator timeout
                case wr of
                    Transition Closed{} -> do
                        logInfo @String $ "Contract Ended"
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract
                    Timeout{} -> do
                        logInfo @String $ "Contract Timeout"
                        continueWith marloweData
                    Transition InputApplied{historyData} -> continueWith historyData
                    Transition Created{historyData} -> continueWith historyData

            CloseContract -> do
                logInfo @String $ "CloseContract"
                let closeContract = do
                        _ <- mkStep params typedValidator timeRange []
                        tell $ Just $ EndpointSuccess reqId AutoResponse
                        marlowePlutusContract

                catching _MarloweError closeContract $ \err -> do
                    logWarn @String $ "Error " <> show err
                    logInfo @String $ "Retry CloseContract in 2 seconds"
                    _ <- awaitTime (time + 2000)
                    continueWith marloweData
            NotSure -> do
                logInfo @String $ "NotSure"
                tell $ Just $ EndpointSuccess reqId AutoResponse
                marlowePlutusContract

          where
            continueWith = autoExecuteContract reqId params typedValidator party
```
This is pays a deposit or waits for other parties to do their part.

### Companion Contract

```haskell
marloweCompanionContract :: Contract CompanionState MarloweCompanionSchema MarloweError ()
marloweCompanionContract = checkExistingRoleTokens
  where
    checkExistingRoleTokens = do
        -- Get the existing unspend outputs of the wallet that activated the companion contract
        pkh <- Contract.ownPaymentPubKeyHash
        let ownAddress = pubKeyHashAddress pkh Nothing
        -- Filter those outputs for role tokens and notify the WebSocket subscribers
        -- NOTE: CombinedWSStreamToServer has an API to subscribe to WS notifications
        utxo <- utxosAt ownAddress
        let txOuts = Ledger.toTxOut <$> Map.elems utxo
        forM_ txOuts notifyOnNewContractRoles
        -- This contract will run in a loop forever (because we always return Right)
        -- checking for updates to the UTXO's for a given address.
        -- The contract could be stopped via /contract/<instance>/stop but we are
        -- currently not doing that.
        checkpointLoop (fmap Right <$> checkForUpdates) ownAddress
    checkForUpdates ownAddress = do
        txns <- NonEmpty.toList <$> awaitUtxoProduced ownAddress
        let txOuts = txns >>= view (citxOutputs . _ValidTx)
        forM_ txOuts notifyOnNewContractRoles
        pure ownAddress

notifyOnNewContractRoles :: TxOut
    -> Contract CompanionState MarloweCompanionSchema MarloweError ()
notifyOnNewContractRoles txout = do
    -- Filter the CurrencySymbol's of this transaction output that might be
    -- a role token symbol. Basically, any non-ADA symbols is a prospect to
    -- to be a role token, but it could also be an NFT for example.
    let curSymbols = filterRoles txout
    forM_ curSymbols $ \cs -> do
        -- Check if there is a Marlowe contract on chain that uses this currency
        contract <- findMarloweContractsOnChainByRoleCurrency cs
        case contract of
            Just (params, md) -> do
                logInfo $ "WalletCompanion found currency symbol " <> show cs <> " with on-chain state " <> show (params, md) <> "."
                tell $ CompanionState (Map.singleton params md)
            Nothing           -> do
            -- The result will be empty if:
            --   * Note [The contract is not ready]: When you create a Marlowe contract first we create
            --                                       the role tokens, pay them to the contract creator and
            --                                       then we create the Marlowe contract.
            --   * If the marlowe contract is closed.
                -- TODO: Change for debug
                logWarn $ "WalletCompanion found currency symbol " <> show cs <> " but no on-chain state."
                pure ()
```

The last interesting contract is the Marlowe Companion Contract.

This is a contract that monitors a participant wallet and notifies when a role token arrives.

It listens to transactions that go to your own address and if there is a token and this token is generated by Marlowe contract creation, it tries to find the Marlowe contract and, if it succeeds, it updates the state of the contract. If you are subscribed to the contract's web socket, you will get a notification about a role token, and you'll get a map of ```MarloweParams``` to ```MarloweData```.

## Brian Bush: The Marlowe CLI

![Screenshot 2022-03-24 at 14-14-17 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/159983312-b23a8223-0501-4025-b084-7a93bc70c983.png)

Today we will be talking about the Marlowe command line tool. We will give you a brief overview and also a brief tutorial.

![Screenshot 2022-03-24 at 14-15-25 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/159983669-7d1fcdde-209a-4a34-a99b-209cd8a46c72.png)

The tool called Marlowe-CLI is made to enable developers to submit transactions and interact with Marlowe contracts from the command line, and do that on the live blockchain; so on one of the testnets. 

An example some of you may be familiar with the Cardano-CLI tool. Which people have used to submit transactions; both simple and advanced Plutus scripts on the blockchain. Marlowe-CLI takes that a step further and lets you submit Marlowe contracts on the blockchain. 

![Screenshot 2022-03-24 at 14-18-21 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/159984163-a42ef1d1-4a87-46d2-9e22-9827663ea61a.png)

There's several use cases and we're just outlining a couple of them here, this is not an exclusive list but gives you an idea of the things that you might be able to do with the Marlowe-CLI tool. 

First, it facilitates interaction and development of Marlowe contracts so you can do things like:

- measure the transaction size 
- submit transactions to the blockchain 
- test your integration with wallets 
- debug validators 

So this is a developer tool, and it's a number of different modes. We will talk about it, but it's for getting into the nitty gritty of operating Marlowe contracts. 

It also gives you early access to Marlowe capabilities on testnet or even the mainnet. You can run Marlowe contracts without a web browser or phone; you  can create your own workflow that operates Marlowe. 

A third use case is to integrate with your own workflow or toolset so you can wrap the Marlowe-CLI tool the same way that folks have wrapped Cardano-CLI to create services such as libraries, faucets, and marketplaces. Furthermore, it can be used for training in the use of Marlowe so people can get a deeper understanding of how Marlowe transactions actually work. 

The model user interface provides an abstract view of those transactions the Marlowe-CLI provides in a very concrete representation, and that's quite close to what's occurring on chain. 

![Screenshot 2022-03-24 at 14-30-11 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/159986112-61e5b798-d2be-4982-bd23-9711ec95bef0.png)

 We have several ways we can use Marlowe-CLI to interact on the blockchain. We are going to mostly focus on the high level method listed here, but we also have two others. 

The high level method is really for supporting workflows where you just want to run a contract at the command line. It hides some of the details of plutus,cardano nodes, and the internals of how input and state work in Marlowe contracts. It really focuses specifically on the contract. 

You can also run the Marlowe-CLI to operate the plutus application backed for Marlowe. There, you are directly interacting with the plutus application back end; which actually has three contracts for Marlowe and several endpoints. This lets you operate at the even higher level, because you don't have to do the UTxO management. Cardano/Daedalus wallets will do the UTxO  work in the contract operation, so this actually very closely mimics the workflow of Marlowe run since Marlowe runs actually uses the plutus application backend. 

There's also a low level mode, that is really for developers that are mostly debugging or really want fine grain controlled over each little operation involved in running a Marlowe contract. You can control and modify the Marlowe state, construct input, look at information about the validators datums/redeemers, and basically all the Plutus processes that are under the hood when you're running a smart contract like Marlowe.


![Screenshot 2022-03-24 at 15-47-39 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/159998044-d44f7d05-9b06-4055-aad8-a5a3dabe571f.png)

This is the simple installation, which installs the way you would install any tool in the cardano ecosystem. 

You have one option to use nix, so if you're using a mix nix system and you have cabal installed:

- just clone the repository  
- open the shell and install the tool 

Otherwise if you have the cabal and ghc tool chains already installed on your machine, you can:

- just clone the repository 
- install Marlowe-CLI 

It will take a little while if you haven't been doing Cardano development, for all the dependencies to download and various packages to build. With a little patience, you will have a working version of Marlowe-CLI after doing the simple installation.


```
$ marlowe-cli --help

Usage: marlowe-cli [--version] COMMAND
  Utilities for Marlowe.

marlowe-cli : a command-line tool for Marlowe contracts

Usage: marlowe-cli [--version] (COMMAND | COMMAND)
  Utilities for Marlowe.

Available options:
  -h,--help                Show this help text
  --version                Show version.

High-level commands:
  run                      Run a contract.
  template                 Create a contract from a template.

Low-level commands:
  contract                 Export contract address, validator, datum, or redeemer.
  input                    Create inputs to a contract.
  role                     Export role address, validator, datum, or redeemer.
  transaction              Create and submit transactions.
  util                     Miscellaneous utilities.
```
More commands found at: [https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe-cli](https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe-cli)


The basic interface for a command line tool, is the command line. Here is an example of just listing all the tools that are available within Marlowe-CLI. We will be focusing on the high level commands today and the high level workflow actually builds on those commands.


![Screenshot 2022-03-24 at 16-03-01 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/160000596-6f3b86a8-83e8-4202-bc59-d801a07c1231.png)


It is built around user interaction, so you might in Marlowe playground either type or graphically design a contract (maybe using blockly or using  Haskell). You would then export that from the playground. Then you have a contract that you would want to run on the network, so using  the Marlowe-CLI you actually have the json file that is coming from Marlowe playground. Also, you have various operations you can use to make sure the contract operates. 

- The first step here at the top this top circle, the user needs to decide on which contract they're going to run and its parameters. 

- If it's a template that's already built into Marlowe-CLI, it's really simple you can just create that json file. If you are not using a template you have to download it from the Marlowe Playground

- Then you initialize the contract and this is basically just bundling information together about which contract you're running, its initial state, who are the people what are the addresses involved in the contract, and such so all that up front work.

- Then you basically have a execute command that actually submits the first transaction of the contract onto the blockchain 

- Finally, you might actually send out some payments. It might just be a single execution where you finished the contract or if the contract is not put complete then the user gets to decide what the next input is for the contract. After they've made that decision, they prepare the next step of the contract and then they execute it.  

What you will see in the example is that once we have done this preliminary work, you are basically doing execute, prepare, and then execute. It will keep going in that circle to advance the contract. This is actually what you'd see visually if you are using Marlowe run; you would see cards that show the contract  progressing so this is mimicking that process.

```
$ marlowe-cli template --help

Usage: marlowe-cli template COMMAND
  Create a contract from a template.

Available options:
  -h,--help                Show this help text

Commands for creating Marlowe contracts from templates:
  escrow                   Create an escrow contract.
  simple                   Create a simple example contract.
  swap                     Create a swap contract.
  zcb                      Create a zero-coupon bond.
```

We have four templates just to get people started:

- the escrow contract
- a very simple contract 
- a swap 
- a zero coupon bond 

These are things you can experiment with at the command line on the testnet. Once you have your contract, either one of those or as we mentioned one you designed in the playground, there are just these couple of commands for running the contract.

```
$ marlowe-cli run --help

Usage: marlowe-cli run COMMAND
  Run a contract.

Available options:
  -h,--help                Show this help text

Commands for running contracts:
  execute                  Run a Marlowe transaction.
  initialize               Initialize the first transaction of a Marlowe contract and write output to a JSON file.
  prepare                  Prepare the next step of a Marlowe contract and write the output to a JSON file.
  withdraw                 Withdraw funds from the Marlowe role address.
```

- execute which is actually putting the Marlowe transaction on the blockchain 
- initializes that preparatory transaction where you're taking your contract and packaging all the information needed to run 
- then before each step you need to prepare the next set of input  
- finally, to get funds outside of Marlowe there's a withdrawal function

These are the four functions one needs to use after you have a contract that may have come from the template or from the playground.

Briefly looking some of the low-level capabilities:


![Screenshot 2022-03-24 at 16-27-21 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/160004567-7bb8b1de-34c2-4f4e-9e4f-16929c12ad83.png)

They allow you do things like create Marlowe; so you actually can compute script addresses, validator hashes, and data hashes. 
You can look at the binary representation of the plutus script, get json representations of the datum or the redeemer, count bytes, look at the execution cost and memory. These things all matter if you're kind of in the nitty gritty world of developing a contract you want to know how much it's going to cost or whether if it is going to fit on the blockchain. Also, maybe even pre-computation of what these hashes are. That creation is supplemented by functions that actually build and submit transactions.


![Screenshot 2022-03-24 at 16-33-06 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/160005452-950a6560-61ac-4dab-ba33-0e6bf8667e32.png)

Those low-level workflows actually come in two forms: 

 - the first is what we're calling a monolithic workflow 

On the left here you give it all the information about the contract, and it bundles that all together so we get this Marlowe file. 
Then using query tools like jq, which is a json query tool you can pull out information about the contract. In this case the,  json file, the Marlowe file is a bundle of comprehensive information about the contract.

If you want to submit transactions, you pull out what you need from it, and you can submit transactions using Cardano-CLI, Blockfrost, or a web service. All the Marlowe magic occurs at the beginning and then you're open to use all the data however you wish. 

![Screenshot 2022-03-24 at 16-49-28 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/160007940-900a0fe0-a9d3-4f05-a050-8658b3caba82.png)

There is a more granular workflow that really does the same thing, except you have individual commands for combining input data needed to run Marlowe into files or addresses; and then once again you can run these with whatever tools you want. 


![Screenshot 2022-03-24 at 16-52-18 PPP 030903 - Alexander Nemish Marlowe in Plutus](https://user-images.githubusercontent.com/59018247/160008343-71c83fc3-e8bc-4ad3-b269-c0d042f6d8c3.png)

A little bit of background of how Marlowe uses Plutus in the Cardano ecosystem.

- The Plutus Script

Marlowe is a Plutus script, so it's a validator and there is actually one Marlowe validator for all Marlowe contracts. In essence, an interpreter; a generic script that can run any Marlowe contract. When the contract is created, this script is specialized for the particular network; whether you are running (testnet or mainnet) and what the currency symbol is for the roles involved in the script. There is a bit of specialization but it's basically a generic validator. 

- The Datum

In the Plutus world, the datum is attached to a UTxO and that datum basically gives state information about it. It restricts how it can be spent; and in the Marlowe world the datum basically contains the part of the actual contract that needs to be executed. While you are executing a Marlowe contract onchain, the datum will get smaller and smaller because there's less and less to do in the future. The datum also contains this state of the contract which has things like balances, a record of the past inputs, and any values of parameters in the contract. You can really think of datum as the state needed to move the contract forward. 

- The Redeemer

The redeemer in the Plutus world is what is given to a transaction in order to spend the UTxO that has the datum. The redeemer in Marlowe sense just has the input for the next step of the contract, so you are making a deposit, choice, or notification. It would simply have that information and  when the transaction is validated on-chain, it is going to look at the datum which is the current state. Then it is going to look at the input, which is in the  redeemer, and it is going to run the Plutus script which is Marlowe. It will then decide whether that input was valid and the contract can proceed to the next step. 

![Screenshot 2022-03-25 at 07-47-57 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160115479-fab16eef-3607-4fe8-8032-9bc1a9657014.png)

We mentioned roles earlier, and roles are very important in the Marlowe validator. In order to understand roles, you should really be familiar with native tokens. Native tokens have a currency symbol and a token name, and Marlowe takes advantage of that. Each contract in Marlowe has a unique currency symbol, and associated with that currency symbol is what's called a validator script for making payments.  

We have the main Marlowe script, and then we also have this subsidiary script just for doing payouts to pay to participants. This is basically a design decision, that was made for security around making payments and authenticating ownership of the funds that will be disbursed. The participants usually have a role token, however this is not always the case and there is a way to use Marlowe contracts without role tokens. The standard use case does use role tokens, and they are basically a Cardano native token that uses the currency symbol of the contract where the token name is the name of the participant; and you will see how this works later on. The participants as we mentioned are the ones withdrawing funds from this payout script, and it enforces that the funds go to the correct person. 


![Screenshot 2022-03-25 at 07-59-16 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160116892-6392639c-5562-49ab-91b2-d1f4776cd515.png)


Now we will plunge into an escrow example that is a nice example of the high-level workflow. 

It involves three participants: we have a buyer that wishes to purchase an item; let's say they're purchasing a bicycle. They are going to deposit funds. After they deposit funds and receive the item they might say "hey there's a problem with this, it didn't arrive" or "it's not as advertised". They have that option, and if they do not report a problem;  then the funds go to the seller. That would be the standard use case. 

We have a flow chart here where you have this deposit of funds: 

- If when the buyer makes their decision => they say everything is alright => the seller receives their funds

- If they do report a problem => the seller might also agree that there's a problem 

There is no real dispute there, and the buyer would just receive the refund

However, the seller might also dispute that there is a problem. If there is a dispute, then a third party comes in (the mediator) and they act as the judge and decide who actually was going to receive the funds. 

Those are the three roles we have:

- the decision about reporting a problem where the buyer makes that decision 
- the seller gets to make a decision about whether or not to dispute if there is a problem and 
- the mediator makes a decision about who is right 

In addition to these decisions, the contract has what are called timeouts. If one of the participants does not actually take the action they need to take, there is a default action that occurs in the contract. For example if the buyer does not report a problem, the contract will not stop there. It  actually waits and then it will proceed. This is one of the features of Marlowe that prevents funds from getting locked in a contract forever, there is basically always a timeout in a contract. 



![Screenshot 2022-03-25 at 08-07-22 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160117987-b68e0a2f-3463-48aa-a4c8-7c10cf88d1ac.png)

If you built this contract in blockly this is what it would look like. We decided to split the screen into three little slices because the contract is too long to fit in a single screen, and still be readable. You can look at Marlowe playground, and try building this for yourself. So this is what the contract looks like in the blockly world, we saw how it looks like as a flowchart. 

![Screenshot 2022-03-25 at 08-30-43 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160121160-ef1672b8-11c6-4986-8a01-341cd92378ca.png)

In Marlowe format, which is basically a Haskell format, you could design the contract this way in the playground. You would see it as plain text, however it is basically doing all the thngs we talked about before and presented as test.

![Screenshot 2022-03-25 at 08-36-59 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160122036-71d5a365-ff8a-4441-9a37-2053428a5116.png)

We mentioned that we had these different paths through the contract. Here are four out of the eight possible outcomes. There are an additional four outcomes more which have to do with the timeouts, however we are not going to go into that today.

- the first one is the buyer says everything is alright 
- the buyer can say that things are not alright there's a problem and the seller confirms the problems 
- the seller disputes, then it goes down to the mediator and in the case where the mediator decides for the seller, we go down here and the seller receives the funds that were deposited by the buyer 
- the last case here is where the mediator decides for the buyer and the buyer gets a refund 
 
These are the cases, and we are going to just look at one of these cases in more detail.


![Screenshot 2022-03-25 at 08-44-08 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160122984-f7e93de9-6c67-4961-8aca-3263a24cb872.png)


We have three roles here, and we have given them names. We are going to look at the case where the seller receives the fund after there's a dispute and  the mediator rules for the seller. The buyer reports a problem, the seller disputes and the mediator dismisses the claim such that the funds go to the seller.

![Screenshot 2022-03-25 at 11-08-50 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160147585-5f3d54e0-b5c0-4b31-b1b1-ca677d40101a.png)

The first stage with Marlowe-CLI is constructing the contract. What you need is:

- the names of the parties
- the deadlines 
- the price of the item
- the Cardano on-chain requirement of minimum ADA 

We have these two validator scripts with two addresses: 

- one is for the contract  
- one is for the payout

Intially they start empty, so we have the full contract containing input, and there are no funds in the contract yet. 


![Screenshot 2022-03-25 at 11-16-09 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160148863-5a0a7ddc-9bc8-46d6-8655-778ef22b7017.png)


There are a lot of parameters here that we are going to be usin, this is just for reference you can refer to these later. We also have detailed tutorials online that goes through all this information.


![Screenshot 2022-03-25 at 11-17-30 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160149077-7fca2d67-743b-4e23-b05c-9d064abf0463.png)


This is what you what you would actually do on the command line with some simple single commands to create the export contract. 

```marlowe-cli template escrow```

- We just say we are going to use the template called escrow, and this is all the input we have to give. 

```--minimum-ada "$MINIMUM_ADA"```

- We are going to give it a three ADA to start off the contract to hold at the script address 

```--price "$PRICE"```

- We are going to say the price is 256 ADA 

```
--seller "Role=$SELLER_ROLE"
--buyer "Role=$BUYER_ROLE"
--mediator "Role=$MEDIATOR_ROLE"
```

- we give the names of the seller, buyer, and mediator
 
```
--payment-deadline "$PAYMENT_DEADLINE"
   --complaint-deadline "$COMPLAINT_DEADLINE"
   --dispute-deadline "$DISPUTE_DEADLINE"
   --mediation-deadline "$MEDIATION_DEADLINE"
```

- we list these four deadlines that are in the contract 

```
--out-contract-file tx-1.contract
--out-state-file tx-1.state
```
- what this command line tool is going to give us is two things; the actual contract and the actual state. These are both json files where you are going to see the contract and the initial state.

![Screenshot 2022-03-25 at 11-33-13 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160152094-640ddbd6-031d-46fe-ab6b-b690904d145b.png)

The next step is to provide the minimum ADA, and actually get the contract started. In this case, Christopher Marlowe the mediator is going to send 3 ADA to the script address to create the contract.  


![Screenshot 2022-03-25 at 11-34-58 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160152436-48160326-0f2b-4708-b2f3-196e27e196b5.png)

This is a little more complicated this takes two commands but we will walk through them as we mentioned earlier. First, you have to initialize the contract:

```

marlowe-cli run initialize --testnet 1097911963              \
                           --slot-length "$SLOT_LENGTH"      \
                           --slot-offset "$SLOT_OFFSET"      \
                           --roles-currency "$ROLE_CURRENCY" \
                           --contract-file tx-1.contract     \
                           --state-file    tx-1.state        \
                           --out-file      tx-1.marlowe      \
                           --print-stats
```

Then you're going to execute:

```
marlowe-cli run execute --testnet 1097911963                       \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"  \
                        --tx-in "$TX_0_MEDIATOR_ADA"               \
                        --change-address "$MEDIATOR_ADDRESS"       \
                        --required-signer "$MEDIATOR_PAYMENT_SKEY" \
                        --marlowe-out-file tx-1.marlowe            \
                        --out-file tx-1.raw                        \
                        --print-stats                              \
                        --submit=600                               \
```

```--testnet 1097911963 ```

- Initializing it first, you tell it which network we are working on this is the public testnet

```
   --slot-length "$SLOT_LENGTH"      
   --slot-offset "$SLOT_OFFSET"      
```

- we are going to use a couple of network parameters the tool understands the particular network you're working on 

```--roles-currency "$ROLE_CURRENCY"``` 

- The currency symbol for the roles 

```
--contract-file tx-1.contract     
--state-file    tx-1.state       
--out-file      tx-1.marlowe  
--print-stats
```

- Then you  give it both the contract and initial state we produced from the template. It wil take this information, package it all up together, and create this transaction (tx-1.marlowe file). Then to get more information, we are saying ```print-stats```` so we're going to get some statistical information about this transaction  


Once you bundle everything together for the transaction into this Marlowe file, then you can run it on the testnet. 

```--testnet 1097911963 ```

- We again define testnet network magic number  

```--socket-path "$CARDANO_NODE_SOCKET_PATH"```

- The socket path of the cardano-node
 
```--tx-in "$TX_0_MEDIATOR_ADA"```

- We are going to be spending some ADA here, so we need to give it an input UTxO that it can actually use to run the transaction. 

```--change-address "$MEDIATOR_ADDRESS"```

- This is as you remember, we will be depositing 3 ADA and then the mediator is going to get all the leftover ADA. 

```--required-signer "$MEDIATOR_PAYMENT_SKEY"```

- The mediator has to sign the transaction, so they are a required signer 
```
 --marlowe-out-file tx-1.marlowe            
 --out-file tx-1.raw                        
 --print-stats                            
 --submit=600    
```

- We are giving it the tx-1,marlowe file, that had all the information we bundled will submit it for you, so we say we do want to submit it and we are  going to wait up to 600 seconds for the transaction to be confirmed. Once you run execute, you are going to probably wait five or ten seconds until the transaction is confirmed, and then the transaction will actually be on the blockchain. 

![Screenshot 2022-03-25 at 12-03-04 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160157509-121bd28a-06a8-412c-a3a9-a7d4a04769d6.png)


The next step is depositing funds: 

- so the buyer deposits 256 ADA to purchase the bicycle 
- 
That deposit goes into the account of the seller, and the contract is holding that amount so the seller can not access it yet.  Marlowe has a sense of accounts, so every bit of funding that is in the contract is assigned to one of the roles.

![Screenshot 2022-03-25 at 12-39-47 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160163566-bf210ceb-476d-4fd6-b5ca-28cdd7d53e13.png)

Here we have a similar pattern for creating and submitting a transaction. Instead of initialize, since this is a subsequent transaction,  we're using the prepare  command and then once again we use the execute command. 

```
marlowe-cli run prepare --marlowe-file tx-1.marlowe           \
                        --deposit-account "Role=$SELLER_ROLE" \
                        --deposit-party "Role=$BUYER_ROLE"    \
                        --deposit-amount "$PRICE"             \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-2.marlowe               \
                        --print-stats
```

 The prepare command basically just gives the contract input, so we have the state of the contract at this tx-1.marlowe file. After we have done the input, we are going to be ready for transaction two so we have tx-2.marlowe.

This input is a deposit input so we are depositing into the seller account, and we are the buyer making the deposit (256 ADA). Then we're going to tell the transaction that we do not want this transaction to be valid for any time. That is basically the core of this, we are just providing the input of the command line.

```
marlowe-cli run execute --testnet 1097911963                                  \
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-1.marlowe                        \
                        --tx-in-marlowe "$TX_1"#1                             \
                        --tx-in-collateral "$TX_0_BUYER_ADA"                  \
                        --tx-in "$TX_0_BUYER_ADA"                             \
                        --tx-in "$TX_0_BUYER_TOKEN"                           \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-2.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-2.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
```

Most of the execute command is similar to what we saw before. 

  
![Screenshot 2022-03-25 at 12-52-06 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/160165733-b697945d-1f2b-4de7-8260-9e1ffcd57ce9.png)


This is an example output since we had this print statistics flag turned on. 

Here you can see that the first transaction:

- it was using about 0.19 ADA, that was the fee 
- the size of the transaction was 4% of the 16k limit on transaction size 
- it was not running Plutus, so we did not use any execution limit execution steps or memory 

The second transacion was a Plutus transaction so you can see it's quite a bit more expensive:

- it is 1.3 ADA , the fee
- it was actually using 97% of the size the budget for transaction size 
- it was using a modest amount of memory and not a lot of computation 


![Screenshot 2022-03-25 at 13-00-45 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/160167249-e764b617-243a-458c-a4ea-7ee3caa3c62c.png)

As you know, these are real transactions. You can visit a blockchain explorer and watch the transactions occur, such as the actual datum, redeemer and the Plutus script on-chain.


![Screenshot 2022-03-25 at 13-03-56 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160167658-9d136693-e5ad-4534-8a57-f5d25bd7556c.png)


Back to our example, the buyer is not going to report that there is a problem. Therefore, Thomas Middleton is saying "hey there's a problem I want my funds back". What the contract actually does at the script address is it moves the funds from the seller's account into the buyer's account. The buyer once again, can not access these, but they are in his account. The choice of which account the funds live in actually has to do with timeouts and contract design.

 ![Screenshot 2022-03-25 at 13-07-34 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160168202-578f51e9-be65-4554-aef1-854eb59a79ac.png)
 
```
 marlowe-cli run prepare --marlowe-file tx-2.marlowe           \
                        --choice-name "Everything is alright" \
                        --choice-party "Role=$BUYER_ROLE"     \
                        --choice-number 0                     \
                        --invalid-before "$NOW"               \
                        --invalid-hereafter "$((NOW+4*HOUR))" \
                        --out-file tx-3.marlowe               \
                        --print-stats

marlowe-cli run execute -testnet 1097911963                                   \                                     
                        --socket-path "$CARDANO_NODE_SOCKET_PATH"             \
                        --marlowe-in-file tx-2.marlowe                        \
                        --tx-in-marlowe "$TX_2"#1                             \
                        --tx-in-collateral "$TX_2"#0                          \
                        --tx-in "$TX_2"#0                                     \
                        --tx-in "$TX_2"#2                                     \
                        --required-signer "$BUYER_PAYMENT_SKEY"               \
                        --marlowe-out-file tx-3.marlowe                       \
                        --tx-out "$BUYER_ADDRESS+$MINIMUM_ADA+1 $BUYER_TOKEN" \
                        --change-address "$BUYER_ADDRESS"                     \
                        --out-file tx-3.raw                                   \
                        --print-stats                                         \
                        --submit=600                                          \
```

Step 3 things get a little bit simpler here, because a lot of this prepare and execute is boilerplate code. The only thing that really changes here from the second transaction is now we're not making a deposit we're reporting a problem. 


![Screenshot 2022-03-25 at 13-10-56 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160168772-ce4ef8c7-c6fb-43ca-9509-8895c4698044.png)


Transaction four is the seller disputing that there is a problem, so Francis Beaumont is making a choice here. Their declaration that they are disputing it does not actually change the disposition of funds in the contract. It is just notifying the contract to move along. so the contract sort of peels away more of itself. 

![Screenshot 2022-03-25 at 13-15-04 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160169390-52385bb2-5ad2-4210-9223-557475d5d13c.png)


The transaction is very similar to the third transaction here, it's now the seller that's the party making the choice.


![Screenshot 2022-03-25 at 13-17-37 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160169816-1c1b3842-2def-46a4-a284-d6348cc2b742.png)


The mediator comes into the situation, so they have to make their decision. In this case they are deciding in favor of the seller, so Christopher Marlowe is saying "I am dismissing the claim". The seller does get their funds so what the contract does is it puts the funds back in the seller's account and then it actually transfers the funds out of the Marlowe script into the payout script. It is putting 256 ADA at the payout address, and it's ready for the seller to collect it. However, since the contract is over, it is taking the minimum ADA and is giving that back to Christopher Marlowe who had supplied it when the contract was created. We are moving things out of the contract and then there is essentially no contract left,there is just this close statement that is the  end of the contract.


![Screenshot 2022-03-25 at 13-18-31 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160169940-805caa62-3ace-41e0-ac9a-d7eff74916ac.png)



This is another transaction reminiscent of numbers three and four. Here we are saying:
 
- dismiss claim
- the mediator is the party that is making the choice
- they are choosing choice number zero, because that was the one that indicates they are dismissing the claim 


![Screenshot 2022-03-25 at 13-19-26 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160170079-4e2b443e-648a-45c8-bdeb-6573b8cc6352.png)

What we have here is the final sort of cleanup steps of running the contract. The seller is going to withdraw their funds, so they are pulling out their 256 ADA to their address.


![Screenshot 2022-03-25 at 13-20-50 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160170310-13d77c39-ad84-48a3-bffa-5a06ca5a41cf.png)

This uses a different command in the Marlowe-CLI; this is the withdrawal command and it's very similar to the other ones. 

- once again tell which network you're on which transaction you're operating on 
- who you are, so the seller's withdrawing the funds 
- they are putting some UTxOs in, most importantly that they are identifying themselves with their role tokens. 
- that role token is kind of their passport or their identity that gives them authorization to do things with this script
- then we have collateral 
- they have to sign it
- they are getting their role token back, they are getting their change so they actually get their 256 ADA when all is said and done 


![Screenshot 2022-03-25 at 13-22-32 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160170569-d3fff343-d0a3-4557-9cfe-8dcd1542942a.png)

![Screenshot 2022-03-25 at 13-22-56 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160170625-177e188d-4389-4ccb-8319-8b976a712034.png)

Similarly the mediator is withdrawing their 3 ADA, this is essentially an identical pattern for the transaction 



![Screenshot 2022-03-25 at 13-23-14 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160170689-207b76e5-30ef-4ce2-8007-7f19e4cbf1d5.png)


In summary, we had 7 transactions and this is actually a map of the UTxOs involved.


![Screenshot 2022-03-25 at 13-25-14 PPP 030904 - Brian Bush The Marlowe CLI](https://user-images.githubusercontent.com/59018247/160171046-f94a4755-c0b3-4062-84ff-19479919eeb7.png)


The Marlowe-CLI tool is pretty much feature complete, so you can run these high-level workflows and can interact with the plutus application back-end. You can also do the low-level workflows, we are using this for testing and debugging. 

There are a few enhancements we will be adding to it, something called localization of contracts:

- this is something Marlowe supports but the CLI does not support yet
- it will help us reduce contract size 

Most of the other functions we plan to include, are really convenience methods.

## Marlowe Playground Demo

We can get started by visting Marlowe Playground here: [https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/](https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/)


![Screenshot 2022-03-25 at 13-28-56 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160171667-7bde8bdc-d88b-43c0-9d3c-e8cfa177c5a3.png)

You can use the playground in Javascript, Haskell, Marlowe, or you can do it in Blockly.

Blockly is very nice and you do not need any programming experience to do this. 

Some updates have happened since the recording of the lecture. Cardano slots have been changed with POSIXTime, so we can use the techniques we used in lecture 3 to get the correct time for the playground.

In Cardano, the Shelley release is the beginning of time, which occurs at: 1596059091000 

We can use that for slot 0 then calculate the other slots by using Ledger.TimeSlot:

```
Prelude Ledger.TimeSlot Data.Default Week03.Deploy> slotToBeginPOSIXTime def 10

POSIXTime {getPOSIXTime = 1596059101000}

Prelude Ledger.TimeSlot Data.Default Week03.Deploy> slotToBeginPOSIXTime def 20

POSIXTime {getPOSIXTime = 1596059111000}

Prelude Ledger.TimeSlot Data.Default Week03.Deploy> slotToBeginPOSIXTime def 30

POSIXTime {getPOSIXTime = 1596059121000}
```

So we will use:

```
Slot 0:  1596059091000
Slot 10: 1596059101000
Slot 20: 1596059111000
Slot 30: 1596059121000
```
### Blockly

Let's start with "Start in Blockly".

![Screenshot 2022-03-26 at 10-59-43 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245224-a5479a9c-9e61-42f3-b80b-2ae7cda1e05e.png)

This is a graphical editor. We can just click and drop a Marlowe contract together.

As an example let's write a contract where there are three parties - Alice, Bob and Charlie.

The idea is that Alice and Bob deposit an amount of Ada into the contract, let's say 10 Ada, and then Charlie decides whether Alice or Bob gets the total amount. Depending on Charlie's decision either Alice gets 20 or Bob gets 20.

There's always the possibility that one of the three doesn't play along; Alice doesn't make her deposit, Bob doesn't make his deposit, or Charlie doesn't make his choice. In this case everybody should just get reimbursed.

When we start with Blockly, there is a contract and it's just a Close contract, which in this case doesn't do anything. If there was money in internal accounts it would pay back the money to the owners of the accounts.

We want to do something else, so let's first wait for a deposit by Alice.

Because that's an external action that's triggered by one of the parties, in this case Alice, we need the When construct that Simon mentioned.

We can remove the Close contract, slide the When one into its place.

![Screenshot 2022-03-26 at 11-03-20 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245402-c8df9cc9-442a-467b-9f08-796271ebd7cb.png)

Here we see all the slots where other things need to go. We see some fields that we have to set.

We can set a timeout so let's say this deposit by Alice has to happen by slot 10 or in our case POSIX 1596059101000.

![Screenshot 2022-03-26 at 11-06-22 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245524-329194d0-8644-4a74-ac61-274bdc576b96.png)

If it doesn't happen, we can say what should happen afterwards, and there is not really a good choice to do anything except close in that case, so in that case nothing will happen.

![Screenshot 2022-03-26 at 11-07-20 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245560-4f5219fc-e63e-494b-af8e-6841798aa4c1.png)

Here we say what external actions we wait for. Let's say we only wait for one action, namely that Alice makes a deposit.

So we can check for actions and pick the deposit one and slide it in.

![Screenshot 2022-03-26 at 11-08-20 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245602-80c4a469-2a18-46cb-a3b0-5d60f73ff380.png)

We see some slots that we have to fill. First of all, a party who has to make the deposit, and there are two choices - a public key or role.

Let's take role because then we can just say Alice. Normally this would be the name of the role token, so whoever owns that token can incorporate that role.

![Screenshot 2022-03-26 at 11-09-50 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245667-6608a66e-ad7f-4566-8421-5064362bccd7.png)

So Alice makes a deposit. Now the amount. That's a Value and let's say we just pick a constant amount of 10 Ada.

The amount is 10, and the fact that it is Ada must be specified in the currency slot.

![Screenshot 2022-03-26 at 11-10-42 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245706-6cde7f96-e011-4d7c-a449-a1b3f7c4d70d.png)

There's also the option to use tokens than Ada, but let's stick with Ada.

![Screenshot 2022-03-26 at 11-11-43 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245747-fdef5ebc-0380-4f53-804b-32c2bf97d46d.png)

Now there are these internal accounts that also belong to one of the parties, so let's say Alice pays it into her own internal account.

That can be copy/pasted rather than getting it from the Party menu again.

![Screenshot 2022-03-26 at 11-12-42 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245775-eade7161-3b6b-4bee-9a86-d8f02b2454e4.png)


Now we must say what happens next, if Alice makes this deposit. Afterwards we want Bob to make a deposit, so we can start by just copying the whole When block.

![Screenshot 2022-03-26 at 11-13-22 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245800-afd30dae-67f6-4458-8f65-d3b8230a1729.png)

First of all we change the timeout to 20 (POSIXTime 1596059111000) so as to give Bob also 10 slots to do something, and then wherever we have Alice, we now put Bob.

![Screenshot 2022-03-26 at 11-14-49 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245846-67f95c43-59d1-419f-ab14-4a93e193af95.png)

So at this point, if both these actions happen, Alice has deposited 10 into her internal account and Bob has deposited 10 into his external account.

Now we want Charlie to make a choice, and this is again an external action, so again we need a When, but this time it's not a deposit so let's delete the deposit. Then let's change the timeout to 30 (POSIXTime 1596059121000) to give Charlie 10 slots to make his choice.

![Screenshot 2022-03-26 at 11-17-10 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160245904-70561a46-1265-4edd-b138-cd8923543cc3.png)


Now we need a different action. Where earlier we had Deposit, now we pick the Choice action. We can give it a name, let's say Winner. We must say who makes the choice, so that's will be Charlie, and now we must specify what values this choice can have.

![Screenshot 2022-03-26 at 11-21-20 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246037-26104ef0-5ae5-4840-979d-3d99e25081d9.png)


That's numeric so because Charlie is supposed to choose between Alice and Bobs, which is two choices, we can pick arbitrary values like one and two. One for Alice, two for Bob. That's already the default so that's fine.

![Screenshot 2022-03-26 at 11-22-32 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246082-b9a34610-c2bc-4c1b-a1d3-48fa26473267.png)

This allows Charlie to choose one or two.

Then if and when Charlie makes a choice, we continue, and it now depends on the choice that he has made. If he chose Alice then Alice must get all the money, if he chose Bob then Bob must get all the money.

So we will add an If conditional.


![Screenshot 2022-03-26 at 11-24-58 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246170-4b3cbd45-f2a3-47f2-b4f6-e6a885c3ccbf.png)


Then we will add an observation to check if Alice is the winner. The observation we add is the value is equal to observation.

![Screenshot 2022-03-26 at 11-26-09 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246217-621958f3-4051-457d-9d61-1ea034257674.png)

To see if it is Alice, we will use the Choice by option to ask if Charlie's Winner name is equal to Alice.

![Screenshot 2022-03-26 at 11-27-40 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246270-518c21e8-c263-4095-a530-36bd772d1d85.png)

In the then branch we now take a pay contract. The payee is who gets the money - it can be an internal account or it can be an external party. In this case it doesn't matter, because when we close, all the parties get the money from the internal accounts as well.


![Screenshot 2022-03-26 at 11-30-43 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246369-e6200377-c0bd-4e9f-9af4-d4f4b2f1fbcc.png)


So, we'll just pick Alice's internal account.

![Screenshot 2022-03-26 at 11-31-28 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246389-d6fab981-ff9a-4015-ba6f-88450ecf26e8.png)

We will pay 10 Ada.

![Screenshot 2022-03-26 at 11-32-04 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246411-e307c7b8-53b6-49cf-b58b-f287a7941a99.png)


So who pays? It must be an internal account because this pay contract is something that the contract has control over, so it is not an external action. So, payments are triggered from internal accounts and in this case, it is Bob's account.

![Screenshot 2022-03-26 at 11-32-58 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246453-3a20fcae-2be6-448e-867c-f03b6bc6d41a.png)

So this now says: If Charlie picked 1, then pay from Bob's internal account 10 Ada to Alice's internal account.

After this we can just close. And when we close, all the internal accounts will be paid to the external owners. At this point, Alices internal account will have 20 Ada, and when we close, she will get the 20 Ada paid to her.

![Screenshot 2022-03-26 at 11-33-56 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246491-28887a31-5043-4bb7-8191-f361d324b978.png)


And, if Charlie did not choose Alice, then we must pay to Bob. We can copy paste the previous Pay contract for this and make the necessary modifications.

![Screenshot 2022-03-26 at 11-38-42 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246738-9c96717b-245a-4dca-bc7b-b5774ecdf90a.png)

And this should be enough for our contract.

Now we can, for example, look at the pure Marlowe. This is the value of the Marlowe data type called Contract.


![Screenshot 2022-03-26 at 11-43-50 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246907-6b0d0945-3762-4ca1-ac0f-16ec14725dd7.png)


And we can send it to the simulator.

![Screenshot 2022-03-26 at 11-44-41 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246936-d0f6fb00-cf35-4783-b9ac-8b99502715de.png)

We can start the simulation.

Now, whenever there is a When, we get prompted for which of the available actions to take. In our case we only ever have one available action at each point.

![Screenshot 2022-03-26 at 11-45-14 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246956-f8c7d876-d787-45de-9210-4f584b2b4e9d.png)


So in the first When, either Alice makes her deposit, or the timeout is reached.

If we wait for the timeout it is very boring. The contract is reduced to Close, and nothing happened.

![Screenshot 2022-03-26 at 11-46-01 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160246991-c4b27396-f771-498f-8ccf-79cacaffd223.png)

If, however, she makes the deposit, then this contract simplifies - it reduces to what happens after she makes the deposit.


![Screenshot 2022-03-26 at 11-48-15 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247088-847ffe07-a262-4198-9645-5699fdae988c.png)

And now we can see we are in the second When, where we are waiting for Bob's deposit. Again, he can choose not to deposit. If he does that, then we can see the actions in the transaction log that Alice deposited 10 Ada and the contract pays this back to Alice upon close.

![Screenshot 2022-03-26 at 11-49-12 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247128-f0da3491-bb45-4c43-b356-13f90d448842.png)

It is more interesting though if Bob also makes his deposit.

![Screenshot 2022-03-26 at 11-49-45 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247144-0c97c04e-cc74-480e-8f03-a54fa25c50b7.png)

Now the contract has simplified again. Now we are in the When where the only available action is that Charlie chooses a winner.

If Charlie doesn't do anything and the contract times out, Bob and Alice both get their money back.

![Screenshot 2022-03-26 at 11-50-32 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247179-f6a6fa71-84e0-45ad-99b6-4cd1950f0e8d.png)


If Charlie picks Alice (choice 1), then we see that the contract pays 20 Ada to Alice.

![Screenshot 2022-03-26 at 11-51-01 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247198-cc7aa195-34b4-443d-b0d7-314aec900392.png)


If instead he picks choice 2, then the contract pays 20 Ada to Bob.

![Screenshot 2022-03-26 at 11-51-50 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247240-10b0d86e-3eb9-4206-b479-17da737b4bd6.png)

Let's now reset the contract.

We will copy the Marlowe code to the clipboard, then create a new Haskell project.

In the Haskell editor there is a template.

![Screenshot 2022-03-26 at 11-55-56 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247420-ed8972a7-4d94-4045-88c5-6232345518de.png)

All this program does is to take a Marlowe contract, and then pretty prints it. This is then used to, for example, run in the simulator.

Instead of Close, we can paste what we just copied to the clipboard.

We can then compile this, and send it to the simulator and it should behave exactly as before.

![Screenshot 2022-03-26 at 11-56-41 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160247459-5caeee98-fcee-40d4-9dfb-6faac445e8f5.png)

There we don't really see the benefit of doing it in Haskell, we could just as well do it in Blockly, although you may find that Blockly is really only useful for learning and writing extremely simple contracts. We have just written a simple contract and already it was starting to get quite unwieldy in the Blockly editor. If you do something more complicated it can start to get very confusing in the editor.

But, we can do other things in this Haskell program. We don't have to literally define a contract. We can use the whole power of Haskell to help us to write the contract.

For example, we can see a lot of repetition because we always have the Alice, Bob and Charlie roles. We could define these separately.

```haskell
{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
alice, bob, charlie :: Party
alice   = 'Alice'
bob     = 'Bob'
charlie = 'Charlie'
```

Note that we can use overloaded string literals here because the fromString function uses the Role constructor for Party.

We can also define a constant for the deposit of 10 Ada.

For (Token "" ""), we can replace this with the ada abbreviation.

We can also simplify Charlie's ChoiceId.

Now it is already cleaned up quite a bit.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract

{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
alice, bob, charlie :: Party
alice   = "Alice"
bob     = "Bob"
charlie = "Charlie"

deposit :: Value
deposit = Constant 10

choiceId :: ChoiceId
choiceId = ChoiceId "Winner" charlie   

contract :: Contract
contract =
    When
        [Case
            (Deposit
                alice
                alice
                ada
                deposit
            )
            (When
                [Case
                    (Deposit
                        bob
                        bob
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Choice
                                choiceId
                                [Bound 1 2]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue choiceId)
                                    (Constant 1)
                                )
                                (Pay
                                    bob
                                    (Account alice)
                                    ada
                                    deposit
                                    Close 
                                )
                                (Pay
                                    alice
                                    (Account bob)
                                    ada
                                    deposit
                                    Close 
                                )
                            )]
                        1596059121000 Close 
                    )]
                1596059111000 Close 
            )]
        1596059101000 Close 
```

It's possible to do more sophisticated things. Our contract is slightly asymmetric even though it sounds like a symmetric situation. Alice and Bob are completely symmetric, but in our contract, Alice has to deposit first.

What we could do is to allow Bob to deposit first as well. In the outermost When we would have two deposits - one where Alice deposits, and one where Bob deposits.

Let's make a little helper function. It takes two Partys - the party that deposits first and the party that deposits second and then it returns a Case. We can use this to parameterise Alice and Bob as x and y in the Case. Note that we only need to do this for the deposits, the part where Charlie makes his choice can remain the same, with Alice and Bob continuing to be represented by 1 and 2 respectively.

Now we can replace the originally-pasted code with our helper function, and we can create the symmetric case where Bob deposits first as an option to the outermost When.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
alice, bob, charlie :: Party
alice   = "Alice"
bob     = "Bob"
charlie = "Charlie"

deposit :: Value
deposit = Constant 10

choiceId :: ChoiceId
choiceId = ChoiceId "Winner" charlie   

contract :: Contract
contract =
    When
        [ f alice bob
        , f bob alice 
        ]
        1596059101000 Close 
  where
    f :: Party -> Party -> Case
    f x y =
        Case
            (Deposit
                x
                x
                ada
                deposit
            )
            (When
                [Case
                    (Deposit
                        y
                        y
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Choice
                                choiceId
                                [Bound 1 2]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue choiceId)
                                    (Constant 1)
                                )
                                (Pay
                                    bob
                                    (Account alice)
                                    ada
                                    deposit
                                    Close 
                                )
                                (Pay
                                    alice
                                    (Account bob)
                                    ada
                                    deposit
                                    Close 
                                )
                            )]
                        1596059121000 Close 
                    )]
                1596059111000 Close 
            )
```

This should compile and we can now send to the simulator.

Now we have two possible actions that can happen in the first step. Alice can deposit 10, or Bob can deposit 10.

![Screenshot 2022-03-26 at 12-29-40 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160248648-a6be33f8-3a4d-4389-a4b6-b0c158a47918.png)


If Bob starts...

![Screenshot 2022-03-26 at 12-30-36 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160248670-e4f8c6c0-a440-46c8-ada3-409980cb07f1.png)

Then now it is Alice's turn.

![Screenshot 2022-03-26 at 12-31-04 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160248677-ea51e233-d13a-4be2-ac3a-df6f24b47a12.png)

![Screenshot 2022-03-26 at 12-31-31 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160248698-96adaa75-56c7-47ed-b6d7-695a845ce7e4.png)

So, basically, to use the Haskell editor, we write a program that produces something of type Contract and you can use all the features of Haskell like local functions or whatever to make your life easier.

When using Blockly, if we had wanted to give Bob the option of being the first to deposit, we would have had no choice but to have copy and pasted the whole When construct.

Of course, there are other options when using Haskell. We could also paramterise the contract, for example, we could pass in the deposit value as an argument.

We could also parameterise the parties and even generalise it so that the number of parties could be variable. This would be very inconvenient if we were to have to do this by hand using Blockly, but in Haskell it is quite straightforward.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Example where

import Language.Marlowe.Extended

main :: IO ()
main = print . pretty $ contract $ Constant 50


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
alice, bob, charlie :: Party
alice   = "Alice"
bob     = "Bob"
charlie = "Charlie"

choiceId :: ChoiceId
choiceId = ChoiceId "Winner" charlie   

contract :: Value -> Contract
contract deposit =
    When
        [ f alice bob
        , f bob alice 
        ]
        1596059101000 Close 
  where
    f :: Party -> Party -> Case
    f x y =
        Case
            (Deposit
                x
                x
                ada
                deposit
            )
            (When
                [Case
                    (Deposit
                        y
                        y
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Choice
                                choiceId
                                [Bound 1 2]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue choiceId)
                                    (Constant 1)
                                )
                                (Pay
                                    bob
                                    (Account alice)
                                    ada
                                    deposit
                                    Close 
                                )
                                (Pay
                                    alice
                                    (Account bob)
                                    ada
                                    deposit
                                    Close 
                                )
                            )]
                        1596059121000 Close 
                    )]
                1596059111000 Close 
            )
```

![Screenshot 2022-03-26 at 12-35-33 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160248848-da4af811-ab1c-4622-bc90-97c68e37c00d.png)

What is noteworthy here is that Marlowe, in contrast to Plutus, is very simply Haskell. The Marlowe team made a point of using only basic Haskell features. You don't need lenses, you don't need Template Haskell, you don't even need monads or type-level programming.

Marlowe is not always appropriate because it is specifically for financial contracts, but if it is appropriate then it is a very nice option due to all the safety assurances that Simon mentioned and because it is much simpler and easy to get right than Plutus.

## Homework

The objective for the homework this week, is to alter the contract we created in the lecture. We need to change the contract such that:

- Charlie makes the first deposit, for an amount of 20 ADA
- If Charlie does not make choice before the contract times out, he needs to pay Alice and Bob 10 ADA each from his deposit


![Screenshot 2022-03-28 at 10-42-52 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160423680-10cbecb7-7960-4555-99f2-4898d7bf351f.png)


We first need to add the desposit Charlie is making, which in this case would be 20 ADA.

![Screenshot 2022-03-28 at 10-45-42 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160424268-ab2f69cc-8762-42c1-b44a-901f27da6ffb.png)


Next, we need to add the case of a timeout. Which means that Charlie needs to pay both Alice and Bob since he did not act. So after slot 40 (POSIXTime 1596059131000), we can add that Charlie will pay both Bob and Alice 10 ADA. The main contract is exactly the same as the lecture, we can just add the time out clause after adding in the rest of the contract from the lecture

The whole contract should look like:


![Screenshot 2022-03-28 at 10-46-22 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160424365-99e6739b-b4b9-4545-b591-ca800f8b77b1.png)


![Screenshot 2022-03-28 at 10-47-00 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160424502-0673897b-0276-434b-8b94-aa6c131b31bf.png)

![Screenshot 2022-03-28 at 10-47-45 https __marlowe-playground-staging plutus aws iohkdev io](https://user-images.githubusercontent.com/59018247/160424607-cb76f55b-8f24-4609-8241-c932137952e8.png)
