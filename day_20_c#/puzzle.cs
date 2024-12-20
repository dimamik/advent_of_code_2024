using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int rows = lines.Length;
        int cols = lines[0].Length;

        char[,] grid = new char[rows, cols];
        (int r, int c) start = (-1, -1);
        (int r, int c) end = (-1, -1);

        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                grid[i, j] = lines[i][j];
                if (grid[i, j] == 'S') start = (i, j);
                if (grid[i, j] == 'E') end = (i, j);
            }
        }

        int[] dr = { -1, 1, 0, 0 };
        int[] dc = { 0, 0, -1, 1 };

        int[,] distNormal = BFS(grid, start, passThroughWalls: false);
        int T_no_cheat = distNormal[end.r, end.c];

        int[,] distToEnd = BFS(grid, end, passThroughWalls: false);

        int part1Answer = SolveWithCheat(grid, start, end, distNormal, distToEnd, maxCheatSteps: 2, T_no_cheat: T_no_cheat);
        Console.WriteLine("Part 1 answer: " + part1Answer);

        int part2Answer = SolveWithCheat(grid, start, end, distNormal, distToEnd, maxCheatSteps: 20, T_no_cheat: T_no_cheat);
        Console.WriteLine("Part 2 answer: " + part2Answer);
    }

    static int[,] BFS(char[,] grid, (int r, int c) start, bool passThroughWalls)
    {
        int rows = grid.GetLength(0);
        int cols = grid.GetLength(1);
        int[,] dist = new int[rows, cols];
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                dist[i, j] = -1;

        Queue<(int r, int c)> q = new Queue<(int r, int c)>();
        dist[start.r, start.c] = 0;
        q.Enqueue(start);

        int[] dr = { -1, 1, 0, 0 };
        int[] dc = { 0, 0, -1, 1 };

        while (q.Count > 0)
        {
            var (r, c) = q.Dequeue();
            for (int k = 0; k < 4; k++)
            {
                int nr = r + dr[k];
                int nc = c + dc[k];
                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                if (dist[nr, nc] >= 0) continue;

                if (!passThroughWalls && grid[nr, nc] == '#') continue;

                dist[nr, nc] = dist[r, c] + 1;
                q.Enqueue((nr, nc));
            }
        }

        return dist;
    }

    static int SolveWithCheat(char[,] grid, (int r, int c) start, (int r, int c) end, int[,] distNormal, int[,] distToEnd, int maxCheatSteps, int T_no_cheat)
    {
        int rows = grid.GetLength(0);
        int cols = grid.GetLength(1);

        List<(int r, int c)> trackCells = new List<(int r, int c)>();
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                if (grid[i, j] != '#') trackCells.Add((i, j));
            }
        }

        var cheatSavings = new Dictionary<(int, int, int, int), int>();

        foreach (var sc in trackCells)
        {
            int startDist = distNormal[sc.r, sc.c];
            if (startDist < 0) continue;

            int[,,] distCheat = new int[rows, cols, maxCheatSteps + 1];
            for (int rr = 0; rr < rows; rr++)
                for (int cc = 0; cc < cols; cc++)
                    for (int s = 0; s <= maxCheatSteps; s++)
                        distCheat[rr, cc, s] = -1;

            distCheat[sc.r, sc.c, 0] = 0;
            Queue<(int r, int c, int s)> q = new Queue<(int r, int c, int s)>();
            q.Enqueue((sc.r, sc.c, 0));

            int[] dr = { -1, 1, 0, 0 };
            int[] dc = { 0, 0, -1, 1 };

            while (q.Count > 0)
            {
                var (cr, cc, cs) = q.Dequeue();
                int dHere = distCheat[cr, cc, cs];
                if ((cr != sc.r || cc != sc.c)
                    && grid[cr, cc] != '#' && cs > 0)
                {
                    int endDist = distToEnd[cr, cc];
                    if (endDist >= 0)
                    {
                        int totalWithCheat = startDist + cs + endDist;
                        int saving = T_no_cheat - totalWithCheat;

                        var key = (sc.r, sc.c, cr, cc);
                        if (!cheatSavings.ContainsKey(key) || cheatSavings[key] < saving)
                            cheatSavings[key] = saving;
                    }
                }

                if (cs < maxCheatSteps)
                {
                    for (int k = 0; k < 4; k++)
                    {
                        int nr = cr + dr[k];
                        int nc = cc + dc[k];
                        if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
                        if (distCheat[nr, nc, cs + 1] < 0)
                        {
                            distCheat[nr, nc, cs + 1] = dHere + 1;
                            q.Enqueue((nr, nc, cs + 1));
                        }
                    }
                }
            }
        }

        int count = 0;
        foreach (var kv in cheatSavings)
        {
            int saving = kv.Value;
            if (saving >= 100) count++;
        }

        return count;
    }
}
