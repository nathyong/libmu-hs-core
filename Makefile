all:
	cd refimpl/ && sbt update
	cd refimpl/ && sbt compile
	cd refimpl/cbinding/ && sbt compile
	cd refimpl/cbinding/ && make JAVA_HOME=/usr/lib/jvm/java-8-openjdk/
	cabal configure --enable-tests
	cabal build

clean:
	cd refimpl/ && sbt clean
	cd refimpl/cbinding/ && make veryclean JAVA_HOME=/usr/lib/jvm/java-8-openjdk/
