const fs = require('fs');

const MOVES = { 'v': [1, 0], '^': [-1, 0], '>': [0, 1], '<': [0, -1] };

function readInput(filename, doubleWide) {
    const content = fs.readFileSync(filename, 'utf-8');
    const [mapPart, movesPart] = content.split('\n\n').map(part => part.trim());

    const lines = mapPart.split('\n').map(line => line.trim());
    const grid = doubleWide
        ? lines.map(line => line.split('').flatMap(char => {
            const widen = { '#': '##', 'O': '[]', '.': '..', '@': '@.' };
            return widen[char].split('');
        }))
        : lines.map(line => line.split(''));

    const moves = movesPart.split('\n').join('').trim();
    return { grid, moves };
}

function findStart(grid) {
    for (let row = 0; row < grid.length; row++) {
        const col = grid[row].indexOf('@');
        if (col !== -1) return [row, col];
    }
    throw new Error("Start not found");
}

function moveAndPush(grid, startR, startC, dr, dc) {
    const stack = [];
    const path = [[startR, startC]];
    const visited = new Set();

    while (path.length > 0) {
        const [r, c] = path.pop();
        const key = `${r},${c}`;

        if (visited.has(key) || grid[r][c] === '.') continue;
        visited.add(key);

        if (grid[r][c] === '#') return [startR, startC];

        stack.push([grid[r][c], r, c]);
        path.push([r + dr, c + dc]);

        if (grid[r][c] === '[') path.push([r, c + 1]);
        if (grid[r][c] === ']') path.push([r, c - 1]);
    }

    stack.sort((a, b) => {
        if (dr) return dr > 0 ? a[1] - b[1] : b[1] - a[1];
        return dc > 0 ? a[2] - b[2] : b[2] - a[2];
    });

    while (stack.length > 0) {
        const [char, oldR, oldC] = stack.pop();
        grid[oldR + dr][oldC + dc] = char;
        grid[oldR][oldC] = '.';
    }

    return [startR + dr, startC + dc];
}

function solve(filename, doubleWide) {
    const { grid, moves } = readInput(filename, doubleWide);
    let [r, c] = findStart(grid);

    for (const move of moves) {
        const [dr, dc] = MOVES[move];
        [r, c] = moveAndPush(grid, r, c, dr, dc);
    }

    let sum = 0;
    grid.forEach((row, rowIndex) => {
        row.forEach((cell, colIndex) => {
            if (cell === '[' || cell === 'O') {
                sum += 100 * rowIndex + colIndex;
            }
        });
    });

    console.log(grid.map(row => row.join('')).join('\n'));
    return sum;
}

console.log("Part 1:", solve("input.txt", false));
console.log("\nPart 2:", solve("input.txt", true));
