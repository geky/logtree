#!/bin/bash

set -eo pipefail
trap 'kill 0' EXIT

if [ $# -lt 1 ]
then
    echo "usage: $0 <result_dir>"
    exit 1
fi

RESULTS="$1"
mkdir -p "$RESULTS"

i=0
for case_ in \
        appends \
        lookups \
        traversal \
        updates \
        removes
do
    for order in \
            random \
            in_order \
            reversed
    do
        i=$((i + 1))
        ./bench.py $case_ $order "$RESULTS/${case_}_${order}.csv" &
    done
done

# 2-level orders only useful for some cases
for case_ in \
        updates \
        removes
do
    for order in \
            in_order_then_reversed \
            reversed_then_in_order
    do
        i=$((i + 1))
        ./bench.py $case_ $order "$RESULTS/${case_}_${order}.csv" &
    done
done

echo "running $i permutations..."
wait
echo "done! $i permutations"
