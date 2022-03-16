# Plutus API documentation

## Playground Server

The haddock documentation can be accessed from the plutus playground website without running it locally at:

[https://playground.plutus.iohkdev.io/doc/haddock/](https://playground.plutus.iohkdev.io/doc/haddock/)


## (Optional) Serving docs from a local server

Head to the plutus-apps directory and run ```nix-shell```:

 - Directory ```totinj@penguin:~/plutus-apps$ ```
```
nix-shell
```

Then while in nix-shell, run the  ```build-and-serve-docs``` command:

 - Directory ```[nix-shell:~/plutus-apps]$```
```
build-and-serve-docs
```
```
Output:
Serving HTTP on 0.0.0.0 port 8002 (http://0.0.0.0:8002/) ...
```
Once you are hosting the server, the general docs can now be accessed at:

[http://0.0.0.0:8002/](http://0.0.0.0:8002/)

The haddock documentation can be accessed at:

[http://0.0.0.0:8002/haddock/](http://0.0.0.0:8002/haddock/)


## How to use the documentation

The plutus documentation is a big static webpage and It includes many modules not directly related with Plutus Contracts, like Plutus Core, Plutus IR or the Playground backend. If you launch haddock from a web server, you can press `s` to open the _search prompt_ which is super handy to discover unknow functions and to find the module a function is exported from.
