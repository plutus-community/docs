# About this project

## Welcome to Plutus Community Documentation site

This documentation site is built on community members contributions, usually Plutus Pioneers that graduated for previous cohorts, and that are willing to help new ones by providing updated notes about their experiences following the course. 

Please don't expect that everything works out of the box, all the content is completely based on some community members best efforts. Don't hesitate to join us if you have something to share with others. 

Until now this website proved to be a very valuable source of information for past cohorts, mainly because:
- the fact that Plutus is quickly evolving. 
- the lack of extensive and updated official documentation.  
- toolset is often unknown for users when joining a new cohort, leading to some frustration, just trying to setup the environment. 
- the environment setup instructions may differ between platforms.

## How to collaborate

Any collaboration is welcome, it doesn't need to be a new document, corrections to the old ones are much needed. To do so, just modify or create a document in the corresponding directory inside "docs", and make a pull request using the community repository:  

https://github.com/plutus-community/docs/ 

In the case of contributing a fix, please make sure to include a description of the issue fixed to facilitate the task of the moderator. Once it is approved, it'll be automatically published and linked into the ToC of the site (for now, using readthedocs.io service thru mkdocs).

## Style Guide

At the momment we are using Markdown language for the documentation for its simplicity and broad adoption. It is also supported by Github's editor, so as we're using Github for version and access control mechanisms of the documentation, it is an easy match. More information on Markdown can be found here :  

https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax

To help standarize the content, we recommend to write the articles following the schema below:
- Title: using #, must be short and descriptive, including OS and technology when aplicable. Be aware that this string will be used by mkdocs to generate the navigation menu, so try to be as specific as possible.
- Author, contact, date of creation, date of last revision.
- Brief description.
- Sections (##) Subsections (###) and content: as a general rule, avoid using pictures unless it is strictly necessary. If you do, upload it to the "img" directory, and name it using snake_case_format_XX.ext using the title of the article for easy identification. Use much plain text as possible, it is the preferred option as it can be copied, pasted, and searched internally and externally to the site. Make use of \``` \``` for marking code snippets. 
- Changelog (optional): Brief summary of changes over time.
- Link to a dedicated forum thread (optional) : Link to a new post on Cardano's forum announcing the new article, that can be used to interact with other users, overcoming this way the static nature of the documentation site.  


## Other resources

Plutus Pioneers Course Materials:  
https://github.com/input-output-hk/plutus-pioneer-program

Learn You a Haskell introduction:  
http://learnyouahaskell.com/

Haskell and Cryptocurrencies course:  
https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm

Discord:  

Cardano Forum:  
https://forum.cardano.org/

Stack Exchange:  
https://cardano.stackexchange.com/

Cardano Documentation:  
https://docs.cardano.org/

Developer Portal:  
https://developers.cardano.org/

IOHK Youtube  
https://www.youtube.com/channel/UCBJ0p9aCW-W82TwNM-z3V2w

Charles Hoskinson's Youtube  
https://www.youtube.com/channel/UCiJiqEvUZxT6isIaXK7RXTg

On Github

- IO Top-Level https://github.com/input-output-hk

- Cardano-node https://github.com/input-output-hk/cardano-node

- Plutus https://github.com/input-output-hk/plutus

- Plutus Applications https://github.com/input-output-hk/plutus-apps

- Plutus Docs https://plutus-apps.readthedocs.io/

- Marlowe https://github.com/input-output-hk/marlowe-cardano
