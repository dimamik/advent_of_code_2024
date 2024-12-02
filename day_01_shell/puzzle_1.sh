#!/bin/bash

# Execute with ./puzzle_1.sh input.txt

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

input_file="$1"

if [ ! -f "$input_file" ]; then
    echo "File not found: $input_file"
    exit 1
fi

sorted_file=$(mktemp)

col1=$(awk '{print $1}' "$input_file" | sort -n)
col2=$(awk '{print $2}' "$input_file" | sort -n)

paste <(echo "$col1") <(echo "$col2") > "$sorted_file"

sum_diff=0
while read -r x y; do
    diff=$((x - y))
    abs_diff=${diff#-}  # Get absolute value
    sum_diff=$((sum_diff + abs_diff))
done < "$sorted_file"

echo "Sum of differences: $sum_diff"
