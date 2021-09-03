# **shamir-secret-sharing**

## General decription

[Shamir's Secret Sharing scheme](https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing) is used to secure a secret in a distributed way. The secret is split into multiple parts, called shares. These shares are used to reconstruct the original secret. One can decide how many shares are generated and how many of these shares are necessary to reconstruct the original data. 

## Installation

If necessary install **stack**:

`$ wget -qO- https://get.haskellstack.org/ | sh`

Other installation modes can be consulted at [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

It is necessary that **stack** is up to date:

`$ stack upgrade`


To build the package it is necessary to change directory inside *shamirs-secret-sharing* and execute:
```
$ stack init
$ stack build
```
And finally it is necessary to install the binaries in a path known by the system.

`$ stack install`

*Note: When executing this last command, the path in which the binary was saved will be displayed on the screen in case you want to delete it later. *

## Usage

Now it is possible to execute the program by typing the command
`shamir-secret-sharing-exe` with the options:

* `c n t plain pts cif` - to encrypt:
  * `n` - The number of shares that are going to be generated.
  * `t` - The number of shares necessary to decrypt.
  * `plain` - Path to the file to be encrypted. 
  * `pts` - Path to file where the n points are goint to be stored.
  * `cif` - Path where the encrypted file is going to be stored. 
  
  In the case in which all the arguments are valid you will be prompted to type a password with at most 30 characters. 
  
* `d pts cif plain` - to decrypt:
  * `pts` - Path to a file which contains at least t shares.
  * `cif` - Path to the encrypted file. 
  * `plain` - Path where the decrypted file is goint to be stored. 

## Documentation

If you want to generate the documentation then execute:

`$ stack haddock`

In the last printed line in the terminal it will be shown the path to `index.html`, which can be opened with any browser to check the documentation.


## Built using
* [Stack](https://docs.haskellstack.org) - Multi-platform program used to develop Haskell projects.
* [Cryptonite](https://github.com/haskell-crypto/cryptonite) - Cryptographic library.

