#!/bin/sh

# If the main executable was compiled with -threaded, Dyre
# should also compile the custom executable with -threaded.

. ../subr.sh

mkdir -p working
cd working

### TEST A ###
cp ../ThreadedTest.hs ../Main.hs ../threadedTest.hs .
$HC -threaded --make Main.hs -o threadedTest || die "compilation failed"
OUTPUT_A=`./threadedTest --dyre-debug`
assert "$OUTPUT_A" "custom True" "A"

echo "Passed"
cd ..
rm -r working
