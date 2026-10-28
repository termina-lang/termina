#!/bin/sh
# Runs the tests of this project with termina test and compares its report with expected.txt
# Usage: test/runner-check/check.sh, with TERMINA set to the termina to check (default: the one on the PATH)

TERMINA="${TERMINA:-termina}"

cd "$(dirname "$0")" || exit 2

# The lines of the report, without the output of make
report() {
    grep -E '^([a-z_]+ \(posix-gcc\)$|  )'
}

failed=0

# Each run has cases that fail, so termina test exits with 1
check() {
    expected="$1"
    shift
    output=$("$TERMINA" test "$@" 2>&1)
    code=$?
    if [ "$code" -ne 1 ]; then
        echo "termina test $*: exit code $code, expected 1"
        failed=1
    fi
    if ! printf '%s\n' "$output" | report | diff -u "$expected" -; then
        echo "termina test $*: the report differs from $expected"
        failed=1
    fi
}

check expected.txt
check expected-isolated.txt outcomes --isolated

if [ "$failed" -eq 0 ]; then
    echo "the report of termina test is the expected one"
fi
exit "$failed"
