all:
	sbt update
	cd microvm-refimpl2/ && sbt compile
	cd microvm-refimpl2/cbinding/ && sbt compile
	cd microvm-refimpl2/cbinding/ && make
	cabal configure --enable-tests
	cabal build

clean:
	cd microvm-refimpl2/ && sbt clean
	cd microvm-refimpl2/cbinding/ && sbt clean
	cd microvm-refimpl2/cbinding/ && make veryclean
