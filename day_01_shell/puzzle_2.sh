#!/bin/bash

# Execute with ./puzzle_2.sh input.txt

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file>"
    exit 1
fi

input_file="$1"

occurrences=$(mktemp)

awk '{count[$2]++} END {for (num in count) print num, count[num]}' "$input_file" > "$occurrences"

similarity_score=0
while read -r left right; do
  occurrence=$(awk -v num="$left" '$1 == num {print $2}' "$occurrences")
  if [[ -n $occurrence ]]; then
    similarity_score=$((similarity_score + left * occurrence))
  fi
done < "$input_file"

echo "Similarity Score: $similarity_score"