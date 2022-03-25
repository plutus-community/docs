# Table of Contents

<!-- toc -->

- [Contributing to Plutus Community](#contributing-to-plutus)
- [Developing Plutus Community](#developing-plutus)
- [Writing documentation](#writing-documentation)
  - [Building documentation](#building-documentation)
  - [Previewing changes locally](#previewing-changes-locally)
  - [Previewing documentation on PRs](#previewing-documentation-on-prs)
  - [Adding documentation tests](#adding-documentation-tests)

<!-- tocstop -->

## Contributing to Plutus Community

---
This site is for the benefit of the Cardano community and is owned and maintained by the community under an open source philosophy. IOG Singapore Pte. Ltd (IOG) is not responsible for, and makes no representations or warranties regarding, the accuracy, reliability and completeness of the aggregated content found in this repo. Any use of the open source Apache 2.0 licensed software is done at your own risk and on a “AS IS” basis, without warranties or conditions of any kind. 
---

Thank you for your interest in contributing to Plutus Community! Before you begin writing code, it is important
that you share your intention to contribute with the team, based on the type of contribution:

1. You want to propose a new feature and implement it.
    - Post about your intended feature in an [issue](https://github.com/input-output-hk/plutus-community/issues),
    and we shall discuss the design and implementation. Once we agree that the plan looks good,
    go ahead and implement it.
2. You want to implement a feature or bug-fix for an outstanding issue.
    - Search for your issue in the [Plutus Community issue list](https://github.com/input-output-hk/plutus-community/issues).
    - Pick an issue and comment that you'd like to work on the feature or bug-fix.
    - If you need more context on a particular issue, please ask and we shall provide.

Once you implement and test our documentation feature or bug-fix, please submit a Pull Request to
https://github.com/input-output-hk/plutus-community-docs.
This site is for the benefit of the Cardano community and is owned and maintained by the community under an open source philosophy. IOG Singapore Pte. Ltd (IOG) is not responsible for, and makes no representations or warranties regarding, the accuracy, reliability and completeness of the aggregated content found in this repo. Any use of the open source Apache 2.0 licensed software is done at your own risk and on a “AS IS” basis, without warranties or conditions of any kind. 
This document covers some of the more technical aspects of contributing
to Plutus Community.  For more non-technical guidance about how to contribute to
Plutus Community, see the [Contributing Guide](docs/source/community/contribution_guide.rst).

## Developing Plutus Community

Developing Plutus Community is a community initiative, Input Output Global is hosting the site. The objective is to share working Plutus . A full set of instructions on installing Plutus from source is here:
https://github.com/input-output-hk/plutus#working-with-the-project

To develop with Plutus on your machine, here are some tips:

**Note** Part of what Nix will be doing is downloading a lot of build artifacts
from IOG to make the compilation processes take a lot less time. It means the
difference between 10 minute builds versus 3+ hour builds. The down-side is
this data will be quite large on your local system and you should expect
another 15G minimum to be used by it. Something to think about before going
further, if more disk space is needed than you have on your system.

### Install Nix and set up cache

Nix can be installed single-user or multi-user

More detailed info can be found in the
[Nix Package Manager Guide](https://nixos.org/manual/nix/stable)


#### single-user

Single-user Nix installation has advantages.
 - No daemon and socket are created
 - A group of 32 nix users doesn't get created on the system
 - Nothing is written into `/etc`

The single-user Nix installer requires curl

```bash
sudo sh -c 'apt update && apt install curl'
```

Install nix

```bash
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

The installer should create the `/nix` directory for you with the proper
permissions. When it's done you will see

```bash
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type

  . /home/<youruser>/.nix-profile/etc/profile.d/nix.sh
```

Execute the above command now to set the environment in this shell (also
logout/login will achieve this)

Make sure the changes the installer just made to your `~/.profile` make sense.

Next we will add IOG's caches to Nix to speed up our development significantly
by using their build artifacts. This is very important and means the difference
between 3+ hours and less than 10 minutes!

```bash
mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
```


#### multi-user

If you decide you'd like to go with multi-user Nix, read on.

The multi-user Nix installer requires curl and rsync

```bash
sudo sh -c 'apt update && apt install curl rsync'
```

Install nix

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

This will run a wizard, prompting you for some things. When it's done we need
to set the environment in this shell (also logout/login will achieve this)

```bash
. /etc/profile.d/nix.sh
```

Next we will add IOG's caches to Nix to speed up our development significantly
by using their build artifacts. This is very important and means the difference
between 3+ hours and less than 10 minutes!

```bash
sudo sh -c "echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> /etc/nix/nix.conf"
sudo sh -c "echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> /etc/nix/nix.conf"
sudo systemctl restart nix-daemon.service
```

Optionally add your shell account to the Nix trusted users in order to apply the
Nix trusted public keys for your builds, e.g.:

```bash
sudo sh -c 'echo "trusted-users = $0" >> /etc/nix/nix.conf' `whoami`
sudo systemctl restart nix-daemon.service
```

#### After either Nix installation method

If everything worked you should be able to run a nix program

```bash
nix-env --version
```

and see output like this, version may vary

```bash
nix-env (Nix) 2.3.14
```


## Writing documentation for Plutus Community

So you want to write some documentation and don't know where to start?
Plutus Community has two main types of documentation:
- user-facing documentation.
These are the docs that you see over at [our docs website](https://plutus.org/docs).
- developer facing documentation.
Developer facing documentation is spread around our READMEs in our codebase and in
the [Plutus Community Developer Wiki](https://github.com/plutus-comnunity/docs/wiki).
If you're interested in adding new developer docs, please read this [page on the wiki](https://github.com/plutus/plutus/wiki/Where-or-how-should-I-add-documentation%3F) on our best practices for where to put it.

The rest of this section is about user-facing documentation.

PyTorch uses [Google style](http://sphinxcontrib-napoleon.readthedocs.io/en/latest/example_google.html)
for formatting docstrings. Length of line inside docstrings block must be limited to 80 characters to
fit into Jupyter documentation popups.

### Building documentation

To build the documentation:

1. Build and install Plutus

2. Install the prerequisites

```bash
cd docs
pip install -r requirements.txt
# `katex` must also be available in your PATH.
# You can either install katex globally if you have properly configured npm:
# npm install -g katex
# Or if you prefer an uncontaminated global executable environment or do not want to go through the node configuration:
# npm install katex && export PATH="$PATH:$(pwd)/node_modules/.bin"
```

> Note that if you are a Facebook employee using a devserver, yarn may be more convenient to install katex:

```bash
yarn global add katex
```

3. Generate the documentation HTML files. The generated files will be in `docs/build/html`.

```bash
make html
```

#### Tips

The `.rst` source files live in [docs/source](docs/source). Some of the `.rst`
files pull in docstrings from Plutus code (for example, via
the `autofunction` or `autoclass` directives). To vastly shorten doc build times,
it is helpful to remove the files you are not working on, only keeping the base
`index.rst` file and the files you are editing. The Sphinx build will produce
missing file warnings but will still complete. For example, to work on `jit.rst`:

```bash
cd docs/source
find . -type f | grep rst | grep -v index | grep -v jit | xargs rm

# Make your changes, build the docs, etc.

# Don't commit the deletions!
git add index.rst jit.rst
...
```


### Previewing changes locally

To view HTML files locally, you can open the files in your web browser. For example,
navigate to `file:///your_plutus_folder/docs/build/html/index.html` in a web
browser.

If you are developing on a remote machine, you can set up an SSH tunnel so that
you can access the HTTP server on the remote machine from your local machine. To map
remote port 8000 to local port 8000, use either of the following commands.

```bash
# For SSH
ssh my_machine -L 8000:my_machine:8000

# For Eternal Terminal
et my_machine -t="8000:8000"
```

Then navigate to `localhost:8000` in your web browser.

**Tip:**
You can start a lightweight HTTP server on the remote machine with:

```bash
plutus -m http.server 8000 <path_to_html_output>
```

Alternatively, you can run `rsync` on your local machine to copy the files from
your remote machine:

```bash
mkdir -p build cpp/build
rsync -az me@my_machine:/path/to/plutus/docs/build/html build
rsync -az me@my_machine:/path/to/plutus/docs/cpp/build/html cpp/build
```

### Previewing documentation on PRs

Plutus Community will host documentation previews at `https://docs-preview.plutus.org/<pr number>/` once the
`plutus_plutus_doc_build` GitHub Actions job has completed on your PR. You can visit that page directly
or find its link in the automated Dr. CI comment on your PR.

### Adding documentation tests

It is easy for code snippets in docstrings and `.rst` files to get out of date. The docs
build includes the [Sphinx Doctest Extension](https://www.sphinx-doc.org/en/master/usage/extensions/doctest.html),
which can run code in documentation as a unit test. To use the extension, use
the `.. testcode::` directive in your `.rst` and docstrings.

To manually run these tests, follow steps 1 and 2 above, then run:

```bash
cd docs
make doctest
```

