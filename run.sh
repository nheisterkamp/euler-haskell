#!/bin/sh
run() {
	PROBLEM="$1"
	ghc $PROBLEM.hs && echo "Running $PROBLEM ..." && time ./$PROBLEM
}

run $@
