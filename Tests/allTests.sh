#!/bin/sh

# Run all test scripts for Dyre.

for TESTDIR in `find . -mindepth 1 -type d`; do
    cd $TESTDIR
    TEST_RESULT=`./runTest.sh`
    if [ "$TEST_RESULT" != 'Passed' ]; then
        echo "$TEST_RESULT in test $TESTDIR"
        rm -r working
        exit 1
    fi
    cd ..
done

echo 'Passed'
