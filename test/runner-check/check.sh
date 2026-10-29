#!/bin/sh
# Runs the tests of this project with termina test and compares its report with expected.txt
# Usage: test/runner-check/check.sh, with TERMINA set to the termina to check (default: the one on the PATH)

TERMINA="${TERMINA:-termina}"

cd "$(dirname "$0")" || exit 2

# The lines of the report, without the output of make nor the frames of a backtrace, whose paths depend on the host,
# and with TIME for the time that a check_elapsed measures, which changes from run to run
report() {
    grep -E '^([a-z_]+ \(posix-gcc\)$|  )' | grep -v -E '^           (backtrace:|  #[0-9]+ )' \
        | sed -E 's/took [0-9]+ s [0-9]+ us, limit/took TIME, limit/'
}

failed=0
output=""

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

# A frame that the backtrace of the last run must have
frame() {
    if ! printf '%s\n' "$output" | grep -q -E "^             #[0-9]+ $1\$"; then
        echo "the backtrace has no frame $1"
        failed=1
    fi
}

check expected.txt
check expected-isolated.txt outcomes --isolated

# The executor of the boards, under the native gdb
if command -v gdb >/dev/null 2>&1; then
    check expected.txt --executor gdb
    check expected-isolated.txt outcomes --isolated --executor gdb
    check expected-exception.txt exception --executor gdb
    frame "runner_at at src/runner/values.fin:8"
    frame "CException__out_of_bounds at tests/exception/test.fin:17"
else
    echo "gdb not found: the executor of gdb is not checked"
fi

if [ "$failed" -eq 0 ]; then
    echo "the report of termina test is the expected one"
fi
exit "$failed"
