to compile and run MuBF, you will need:

sbt version 0.13.9 or above - the scala build tool, you can get it from most package managers however, ubuntu 14 LTS uses an outdated version and you will have to install sbt from its website (http://www.scala-sbt.org/)
sudo pacman -S sbt, sudo apt-get install sbt ...

ghc-7.10, cabal-1.22 - In general, users tend to install the haskell platform available from most package managers, users should check out (https://www.haskell.org/platform/) to install.

openjdk 6, 7 or (preferably) 8 - available on many package managers, as well as from oracle (http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)

haskell packages (hspec, parsec & c-storable-deriving). available from cabal:
cabal update
cabal install hspec parsec c-storable-deriving

clone the repository:
git clone https://github.com/microvm/bf-mu.git

cd mu-bf

you need to manually change MuBF.cabal on the line specified ghc-options: under executable MuBF, change /home/name_here/Documents/git/bf-mu/cbinding/ to point to bf-mu/refimpl/cbinding/ for your system.

Important Note. If using a non linux system, the -Wl,--no-as-needed flag may not be needed (and indeed may break the build process), thus if the build fails the first time. Try again but delete the -optl-Wl,--no-as-needed flag from the MuBF.cabal file (ghc-options section)

for first time use, a makefile has been provided to compile mu refimpl and MuBF (with tests)
export JAVA_HOME=/path/to/java/home
make

This could take a while, since sbt is very slow to update and compile.

to run tests:
cabal test

to test MuBF with a bf program, copy test-program/hello.bf into dist/build/MuBF/

./MuBF -f hello.bf -c -r

This should print a large amount of debug info, and near the bottom. Hello World!

in future, you can run
./MuBF -f file.bf -r | grep -v DEBUG
to remove debug and type check information
