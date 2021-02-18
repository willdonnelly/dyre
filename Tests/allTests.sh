#!/bin/sh

# Run all test scripts for Dyre.

if [ -z "$HC" ]; then
    export HC=ghc
fi

for TESTDIR in `find . -mindepth 1 -type d`; do
    echo "Running $TESTDIR"
    cd $TESTDIR
    TEST_RESULT=`./runTest.sh 2>&1`
    TEST_STATUS=$?
    if [ "$TEST_STATUS" -ne 0 ]; then
        echo "$TESTDIR failed; output:"
        echo "$TEST_RESULT";
        exit 1
    fi
    cd ..
done

echo 'Passed'
