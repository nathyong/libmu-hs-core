to compile and use libmu, you will need:

sbt version 0.13.9 or above - the scala build tool, you can get it from most package managers however, ubuntu 14 LTS uses an outdated version and you will have to install sbt from its website (http://www.scala-sbt.org/)
sudo pacman -S sbt, sudo apt-get install sbt ...

ghc-7.10, cabal-1.22 - In general, users tend to install the haskell platform available from most package managers, users should check out (https://www.haskell.org/platform/) to install.

openjdk 6, 7 or (preferably) 8 - available on many package managers, as well as from oracle (http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

haskell packages (hspec & c-storable-deriving). available from cabal:
cabal update
cabal should automatically install missing dependencies (during cabal build) however if not,
you can install hspec, parsec & c-storable-deriving which are usually missing

clone the repository:
git clone --recursive https://github.com/andrew-m-h/libmu-HS.git

cd libmu-HS

you need to manually change MuBF.cabal on the line specified ghc-options: under executable MuBF, change /home/name_here/Documents/git/bf-mu/microvm-refimpl2/cbinding/ to point to bf-mu/microvm-refimpl2/cbinding/ for your system.

for first time use, a makefile has been provided to compile mu refimpl
export JAVA_HOME=/path/to/java/home
make

This could take a while, since sbt is very slow to update and compile.

Now to compile the libmu library, this can be done using cabal

cabal configure --enable-tests
cabal install

If an error occurs claiming that the linker could not find refimpl2start.so, then you must manually set DYLIB_LIBRARY_PATH.
export DYLIB_LIBRARY_PATH=/path/to/bf-mu/refimpl/cbinding/
This is a known issue with mac and setting -rpath for clang

to run tests:
cabal test

you should now startup ghci and enter

:m + Libmu, tab completionn should yeild a plethora of libmu files
