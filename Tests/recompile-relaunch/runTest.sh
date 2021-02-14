#!/bin/sh

# Tests Dyre's ability to recompile a custom configuration
# upon relaunch, and restore the state again after.

. ../subr.sh

mkdir -p working
cd working

### TEST A ###
cp ../RecompileRelaunchTest.hs ../Main.hs ../recompileRelaunchTest.hs .
$HC --make Main.hs -o recompileRelaunch || die "compilation failed"
OUTPUT_A=`./recompileRelaunch --dyre-debug --deny-reconf`
assert "$OUTPUT_A" "Testing....Successful" "A"

### TEST B ###
OUTPUT_B=`./recompileRelaunch --dyre-debug --deny-reconf`
assert "$OUTPUT_B" "..Successful..Successful" "B"

echo "Passed"
cd ..
rm -r working
