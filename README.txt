to compile and use libmu, you will need:

ghc-7.10, cabal-1.22 - In general, users tend to install the haskell platform available from most package managers, users should check out (https://www.haskell.org/platform/) to install.

haskell packages (hspec & c-storable-deriving). available from cabal:
cabal update
you can install hspec, parsec & c-storable-deriving which are usually missing

clone the repository:
git clone https://github.com/andrew-m-h/libmu-HS.git

cd libmu-HS

cabal configure --enable-tests
cabal install

to run tests:
cabal test

you should now startup ghci and enter

:m + Libmu, tab completionn should yeild a plethora of libmu files
