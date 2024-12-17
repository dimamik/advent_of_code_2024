<?php

function parseInput(string $filename): array
{
    $rawLines = file($filename, FILE_IGNORE_NEW_LINES);
    if (!$rawLines) {
        throw new RuntimeException("Could not read input file.");
    }

    $lines = array_map('trim', $rawLines);

    $registerA = null;
    $registerB = null;
    $registerC = null;

    foreach ($lines as $line) {
        if (preg_match('/^Register A:\s*(-?\d+)$/', $line, $match)) {
            $registerA = (int)$match[1];
        } elseif (preg_match('/^Register B:\s*(-?\d+)$/', $line, $match)) {
            $registerB = (int)$match[1];
        } elseif (preg_match('/^Register C:\s*(-?\d+)$/', $line, $match)) {
            $registerC = (int)$match[1];
        }
    }

    $programLine = null;
    foreach ($lines as $line) {
        if (stripos($line, 'Program:') === 0) {
            $programLine = $line;
            break;
        }
    }

    if (!preg_match('/^Program:\s*(.*)$/', $programLine, $progMatch)) {
        throw new RuntimeException("Failed to parse program from line: " . $programLine);
    }

    $programParts = explode(',', $progMatch[1]);
    $program = array_map('intval', $programParts);

    return [
        'A' => $registerA,
        'B' => $registerB,
        'C' => $registerC,
        'program' => $program
    ];
}

/**
 * Run the given 3-bit computer program with the initial value of A provided.
 * Registers B and C start at 0 for this run, unless you specifically want to modify that.
 * The function returns any values produced by the 'out' instruction.
 *
 * Instructions are defined as follows (each consumes two 3-bit values: opcode, operand):
 * 0: adv - A = A >> (2^(combo(operand)))       [Integer division by 2^(combo value)]
 * 1: bxl - B = B ^ operand (literal)
 * 2: bst - B = (combo(operand) % 8)
 * 3: jnz - if A != 0 then IP = operand (literal), else do nothing
 * 4: bxc - B = B ^ C (operand ignored)
 * 5: out - output (combo(operand) % 8)
 * 6: bdv - B = A >> (2^(combo(operand)))
 * 7: cdv - C = A >> (2^(combo(operand)))
 *
 * Combo operand meaning:
 * 0–3: literal 0–3
 * 4: value in A
 * 5: value in B
 * 6: value in C
 * 7: reserved, won't appear
 *
 * @param int[] $prog The program (sequence of 3-bit ints)
 * @param int   $A    Initial value for register A
 * @return int[] Array of outputs produced by 'out' instructions
 * @throws RuntimeException If invalid combo indices appear.
 */
function runProgram(array $prog, int $A): array
{
    $B = 0;
    $C = 0;
    $outputs = [];

    // This anonymous function returns the value based on the combo operand:
    // 0=0,1=1,2=2,3=3,4=A,5=B,6=C. If operand out of range, error.
    $comboValue = function ($index) use (&$A, &$B, &$C) {
        $comboMap = [0, 1, 2, 3, $A, $B, $C];
        if ($index < 0 || $index > 6) {
            throw new RuntimeException("Invalid combo index: $index");
        }
        return $comboMap[$index];
    };

    $ip = 0; // Instruction pointer, moves in steps of 2 unless jnz jumps

    // Run until IP goes beyond program length
    while ($ip < count($prog)) {
        $op = $prog[$ip];
        $val = $prog[$ip + 1] ?? 0;

        switch ($op) {
            case 0: // adv: A = floor(A / (2^(combo(val))))
                $A = $A >> $comboValue($val);
                break;

            case 1: // bxl: B = B ^ literal(val)
                $B = $B ^ $val;
                break;

            case 2: // bst: B = combo(val) % 8
                $B = $comboValue($val) % 8;
                break;

            case 3: // jnz: if (A != 0) IP = val (literal)
                if ($A != 0) {
                    $ip = $val - 2; 
                    // Note: subtracting 2 because after instruction we will add 2,
                    // effectively making IP = val next loop. This is a quirk of how the code increments IP.
                }
                break;

            case 4: // bxc: B = B ^ C (operand ignored)
                $B = $B ^ $C;
                break;

            case 5: // out: output (combo(val) % 8)
                $outputs[] = ($comboValue($val) % 8);
                break;

            case 6: // bdv: B = floor(A / (2^(combo(val))))
                $B = $A >> $comboValue($val);
                break;

            case 7: // cdv: C = floor(A / (2^(combo(val))))
                $C = $A >> $comboValue($val);
                break;

            default:
                // Invalid opcode: break
                break 2;
        }

        $ip += 2; // Move to next instruction (2 3-bit values per instruction)
    }

    return $outputs;
}

/**
 * Recursively search for an initial A value that makes the program output match the program itself.
 *
 * We don't care about B and C registers, since they don't affect the output.
 * 
 * The function tries all values of a 3-bit chunk (0 through 7) shifted by (3 * $i) bits,
 * building up the A value piece by piece. For each candidate, it runs the program and checks if
 * the output at position $i matches the program[$i]. If it matches, it recurses further down 
 * until $i == 0.
 *
 * @param int[] $prog The program array.
 * @param int   $A    Current candidate for register A.
 * @param int   $i    Current index in the output/program we are matching.
 * @return int The offset found or -1 if no match found.
 */
function recurse(array $prog, int $A, int $i): int
{
    // We try all possible 3-bit values (0..7) for this chunk.
    for ($k = 0; $k < 8; $k++) {
        // This is a hack from observations of the program's behaviour which was 
        // taken from the reddit solutions thread:
        // https://www.reddit.com/r/adventofcode/comments/1hg38ah/2024_day_17_solutions/
        $z = $k << (3 * $i);
        $A_candidate = $A + $z;

        // We skip A=0 if we want strictly positive (this can be removed if not needed)
        if ($A_candidate == 0) {
            continue;
        }

        $result = runProgram($prog, $A_candidate);

        // Check if the output at index $i matches the program's value at $i
        if (isset($result[$i]) && $result[$i] === $prog[$i]) {
            printf("%o -> %s\n", $A_candidate, json_encode($result));
            // If we found a match for this position and $i == 0, we return this offset.
            if ($i == 0) {
                return $z; 
            }

            // Otherwise, recurse further down the outputs
            $lower = recurse($prog, $A_candidate, $i - 1);
            if ($lower >= 0) {
                return $z + $lower;
            }
        }
    }

    // No match found
    return -1;
}

try {
    $inputData = parseInput('input.txt');
    $prog = $inputData['program'];
    $A_init = $inputData['A'];
    $B_init = $inputData['B'];
    $C_init = $inputData['C'];

    echo "program = " . json_encode($prog) . PHP_EOL;
    echo "A = $A_init, B = $B_init, C = $C_init" . PHP_EOL;

    $result = runProgram($prog, $A_init);
    echo "result = " . json_encode($result) . PHP_EOL;

    echo PHP_EOL . str_repeat('*', 40) . PHP_EOL . PHP_EOL;

    $res = recurse($prog, 0, count($prog) - 1);
    echo "dec: $res, oct: " . sprintf("%o", $res) . PHP_EOL;
    echo "Final output for A=$res: " . json_encode(runProgram($prog, $res)) . PHP_EOL;
    echo "Program: " . json_encode($prog) . PHP_EOL;

} catch (Exception $e) {
    fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
    exit(1);
}
