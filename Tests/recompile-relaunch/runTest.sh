#!/bin/sh

# Tests Dyre's ability to recompile a custom configuration
# upon relaunch, and restore the state again after.

# Assert the equality of two strings.
function assert() {
    echo "$1" >&2
    if [ "$1" != "$2" ]; then
        echo "Failed test $3";
        exit 1;
    fi
}

mkdir working
cd working

### TEST A ###
cp ../RecompileRelaunchTest.hs ../Main.hs ../recompileRelaunchTest.hs .
ghc --make Main.hs -o recompileRelaunch 2> /dev/null
OUTPUT_A=`./recompileRelaunch --dyre-debug --deny-reconf`
assert "$OUTPUT_A" "Testing....Successful" "A"

### TEST B ###
OUTPUT_B=`./recompileRelaunch --dyre-debug --deny-reconf`
assert "$OUTPUT_B" "..Successful..Successful" "B"

echo "Passed"
cd ..
rm -r working
