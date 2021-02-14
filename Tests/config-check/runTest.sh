#!/bin/sh

# Tests Dyre's ability to recompile a custom configuration
# upon relaunch, and restore the state again after.

. ../subr.sh

mkdir -p working
cd working

### TEST A ###
cp ../ConfigCheckTest.hs ../Main.hs .
cp ../configCheckTestA.hs ./configCheckTest.hs
$HC --make Main.hs -o configCheck || die "compilation failed"
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
