# Plutus Playground setup for Plutus Pioneer Program

This guide assumes you setup your environment already and have nix installed.

In order to get started with Plutus Playground, we need to have two terminals running, both of which are in the nix-shell.

Let’s get started with terminal 1. Head to the plutus-apps directory and first run nix-shell:


```
Terminal 1

totinj@penguin:~/plutus-apps$ nix-shell
```


Next we head to plutus-playground-server directory and run: 

```
Terminal 1

[nix-shell:~/plutus-apps/plutus-playground-server]$ plutus-playground-server
```

If Successful, you will see the output:

```
Terminal 1

Interpreter Ready
```

Let’s get started with terminal 2. Head to the plutus-apps directory and first run nix-shell:


```
Terminal 2

totinj@penguin:~/plutus-apps$ nix-shell
```


Next we head to plutus-playground-client directory and run: 

```
Terminal 2

[nix-shell:~/plutus-apps/plutus-playground-client]$ npm run start
```

If Successful, you will see the output:

```
Terminal 2

[wdm]: Compiled successfully.

or

[wdm]: Compiled with warnings.
```

Keep both terminals open, and we should now be able to access Plutus Playground from the browser.

Open a browser and head to the address:

```
https://localhost:8009
```

You will get a warning complaining about it being a risky website, ignore the message to click through anyway.

You should now be able to successfully compile and run the auctions contract by using the two buttons in the top right corner: “Compile” and “Simulate”.<br/><br>
