# Assert the equality of two strings.
assert() {
    echo "$1" >&2
    if [ "$1" != "$2" ]; then
        echo "Failed test $3";
        echo " expected: $2"
        echo "      got: $1"
        exit 1;
    fi
}

die() {
    echo "$1" ; exit 1
}
