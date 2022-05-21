# Schnorr authentication protocol in Haskell

Schnorr protocol is a Zero Knowledge Protocol based on the discrete logarithm
problem for identity verification. This repo contains an implementation of an
authentication system using this protocol written in Haskell. Some tests were
performed to evaluate the correctness of the implementation, which were succesful.

## Dependencies

The dependencies for this project can be installed using cabal.

```bash
cabal install cryptonite directory optparse-applicative network system-filepath
```

## Compile

To compile this project use cabal.

```bash
git clone https://github.com/joaosreis/haskell-schnorr.git
cd haskell-schnorr
cabal build
```

The executable file will be located at `dist/build/schnorr`. 
