// tsc puzzle.ts && node puzzle.js
import * as fs from "fs";
import { MinPriorityQueue } from "@datastructures-js/priority-queue"; 

interface State {
  x: number;
  y: number;
  dir: number; // 0=Up,1=Right,2=Down,3=Left
}

interface PQEntry {
  dist: number;
  x: number;
  y: number;
  dir: number;
}

function solve(): void {
  const maze = fs
    .readFileSync("input.txt", "utf8")
    .split("\n")
    .map((l: string) => l.split(""));

  const rows = maze.length;
  const cols = maze[0].length;

  let startX = -1;
  let startY = -1;
  let endX = -1;
  let endY = -1;

  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      const ch = maze[r][c];
      if (ch === "S") {
        startX = r;
        startY = c;
      }
      if (ch === "E") {
        endX = r;
        endY = c;
      }
      if (startX !== -1 && endX !== -1) break;
    }
  }

  const directions = [
    [-1, 0], // Up
    [0, 1], // Right
    [1, 0], // Down
    [0, -1], // Left
  ];

  const INF = Number.POSITIVE_INFINITY;
  const dist = new Array(rows)
    .fill(0)
    .map(() => new Array(cols).fill(0).map(() => new Array(4).fill(INF)));

  // Initialize the priority queue
  const pq = new MinPriorityQueue<PQEntry>((entry) => entry.dist);

  // Dijkstra's algorithm
  dist[startX][startY][1] = 0;
  pq.enqueue({ dist: 0, x: startX, y: startY, dir: 1 });

  while (!pq.isEmpty()) {
    const { dist: curDist, x, y, dir } = pq.dequeue();
    if (dist[x][y][dir] < curDist) continue;

    // Move forward
    const nx = x + directions[dir][0];
    const ny = y + directions[dir][1];
    if (nx >= 0 && nx < rows && ny >= 0 && ny < cols && maze[nx][ny] !== "#") {
      const nd = curDist + 1;
      if (nd < dist[nx][ny][dir]) {
        dist[nx][ny][dir] = nd;
        pq.enqueue({ dist: nd, x: nx, y: ny, dir });
      }
    }

    // Turn left
    // (to not deal with negative values we add 3 instead of subtracting 1)
    const leftDir = (dir + 3) % 4;
    const leftCost = curDist + 1000;
    if (leftCost < dist[x][y][leftDir]) {
      dist[x][y][leftDir] = leftCost;
      pq.enqueue({ dist: leftCost, x, y, dir: leftDir });
    }

    // Turn right
    const rightDir = (dir + 1) % 4;
    const rightCost = curDist + 1000;
    if (rightCost < dist[x][y][rightDir]) {
      dist[x][y][rightDir] = rightCost;
      pq.enqueue({ dist: rightCost, x, y, dir: rightDir });
    }
  }

  let minCost = INF;
  for (let d = 0; d < 4; d++) {
    if (dist[endX][endY][d] < minCost) {
      minCost = dist[endX][endY][d];
    }
  }

  console.log(minCost);

  // Backwards BFS 
  const onPathTiles = new Set<string>();
  const queue: State[] = [];
  const visited = new Array(rows)
    .fill(false)
    .map(() => new Array(cols).fill(false).map(() => new Array(4).fill(false)));

  for (let d = 0; d < 4; d++) {
    if (dist[endX][endY][d] === minCost) {
      queue.push({ x: endX, y: endY, dir: d });
      visited[endX][endY][d] = true;
      onPathTiles.add(`${endX},${endY}`);
    }
  }

  while (queue.length > 0) {
    const { x, y, dir } = queue.pop()!;
    const curDist = dist[x][y][dir];

    const px = x - directions[dir][0];
    const py = y - directions[dir][1];
    if (px >= 0 && px < rows && py >= 0 && py < cols && maze[px][py] !== "#") {
      if (dist[px][py][dir] + 1 === curDist) {
        if (!visited[px][py][dir]) {
          visited[px][py][dir] = true;
          queue.push({ x: px, y: py, dir });
          onPathTiles.add(`${px},${py}`);
        }
      }
    }

    const leftDir = (dir + 1) % 4;
    if (dist[x][y][leftDir] + 1000 === curDist) {
      if (!visited[x][y][leftDir]) {
        visited[x][y][leftDir] = true;
        queue.push({ x, y, dir: leftDir });
        onPathTiles.add(`${x},${y}`);
      }
    }

    const rightDir = (dir + 3) % 4;
    if (dist[x][y][rightDir] + 1000 === curDist) {
      if (!visited[x][y][rightDir]) {
        visited[x][y][rightDir] = true;
        queue.push({ x, y, dir: rightDir });
        onPathTiles.add(`${x},${y}`);
      }
    }
  }

  console.log(onPathTiles.size);
}

solve();
