all:
	sbt update
	cd refimpl/ && sbt compile
	cd refimpl/cbinding/ && sbt compile
	cd refimpl/cbinding/ && make
	cabal configure --enable-tests
	cabal build

clean:
	cd refimpl/ && sbt clean
	cd refimpl/cbinding/ && make veryclean
