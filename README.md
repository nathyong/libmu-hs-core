to use, compile using cabal.

cabal configure
cabal build

the resultant MuIR binary can be called using several command line arguments:

--file, -f file: file to compile or parse 
--output, -o file: if given, the resultant text will be written to the output file specified
--parse, -p: if given, the program will only be parsed and displayed (either to stdout or output file)
--check, -c: if given, the program will be type checked in MuIR format.

e.g.

./MuIR --file hello.bf -c --out output.uir
./MuIR --file hello.bf

