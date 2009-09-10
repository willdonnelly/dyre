#!/bin/sh

# Tests basic compiling and running.
# Tests state persistence across restarts.
# Tests custom configuration recompilation.
# Tests restarting a custom configuration.
# Tests compilation error reporting.

# Assert the equality of two strings.
function assert() {
    if [ "$1" != "$2" ]; then
        echo "Failed test $3";
        exit 1;
    fi
}

### SETUP ###
mkdir working
cd working

### TEST A ###
cp ../BasicTest.hs ../Main.hs .
ghc --make Main.hs -o basic 2> /dev/null
OUTPUT_A=`./basic --dyre-debug`
assert "$OUTPUT_A" "Basic Test Version 1.0 - 3" "A"

### TEST B ###
cp ../goodConfig.hs basicTest.hs
OUTPUT_B=`./basic --dyre-debug`
assert "$OUTPUT_B" "Basic Test Version 2.0 - 3" "B"

### TEST C ###
sleep 1
cp ../badConfig.hs basicTest.hs
OUTPUT_C=`./basic --dyre-debug`
assert "$OUTPUT_C" "Compile Error" "B"

### TEARDOWN ###
echo "All tests passed"
cd ..
rm -r working
