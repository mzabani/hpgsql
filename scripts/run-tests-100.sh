#!/usr/bin/env bash

OUTPUT_FILE="test-output.txt"
FAILED_OUTPUT="last-failed-output.txt"

rm -f "$FAILED_OUTPUT"

passes=0
failures=0

for i in $(seq 1 100); do
    run tests >"$OUTPUT_FILE" 2>&1
    if [ $? -eq 0 ]; then
        echo "$i: PASSED"
        passes=$((passes + 1))
    else
        echo "$i: FAILED"
        failures=$((failures + 1))
        cp "$OUTPUT_FILE" "$FAILED_OUTPUT"
    fi

    if [ $((i % 5)) -eq 0 ]; then
        echo "--- After $i runs: $passes passed, $failures failed ---"
    fi
done

rm -f "$OUTPUT_FILE"

echo ""
echo "=== Final: $passes passed, $failures failed ==="

if [ -f "$FAILED_OUTPUT" ]; then
    echo "Last failed output kept in $FAILED_OUTPUT"
else
    echo "All 100 runs passed!"
fi
