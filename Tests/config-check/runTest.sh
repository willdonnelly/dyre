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
cp ../ConfigCheckTest.hs ../Main.hs .
cp ../configCheckTestA.hs ./configCheckTest.hs
ghc --make Main.hs -o configCheck 2> /dev/null
OUTPUT_A=`./configCheck --dyre-debug`
assert "$OUTPUT_A" "custom-a" "A"

sleep 1

### TEST B ###
cp ../configCheckTestB.hs ./configCheckTest.hs
mv cache/configCheckTest* configCheck
OUTPUT_B=`./configCheck --dyre-debug`
assert "$OUTPUT_B" "custom-a" "B"

echo "Passed"
cd ..
rm -r working
