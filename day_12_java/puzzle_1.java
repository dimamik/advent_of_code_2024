import java.io.*;
import java.util.*;

public class GardenFenceCalculator {

    public static char[][] parseInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line.trim());
            }
        }
        int rows = lines.size();
        int cols = lines.get(0).length();
        char[][] grid = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            grid[i] = lines.get(i).toCharArray();
        }
        return grid;
    }

    public static List<int[]> floodFill(char[][] grid, int x, int y, boolean[][] visited) {
        List<int[]> regionCells = new ArrayList<>();
        Queue<int[]> queue = new LinkedList<>();

        int rows = grid.length;
        int cols = grid[0].length;

        char regionType = grid[x][y];
        queue.add(new int[]{x, y});
        visited[x][y] = true;

        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        while (!queue.isEmpty()) {
            int[] cell = queue.poll();
            int cx = cell[0], cy = cell[1];
            regionCells.add(cell);

            for (int[] dir : directions) {
                int nx = cx + dir[0], ny = cy + dir[1];
                if (nx >= 0 && nx < rows && ny >= 0 && ny < cols &&
                    !visited[nx][ny] && grid[nx][ny] == regionType) {
                    visited[nx][ny] = true;
                    queue.add(new int[]{nx, ny});
                }
            }
        }
        return regionCells;
    }

    public static int[] calculateAreaAndPerimeter(char[][] grid, List<int[]> regionCells) {
        int area = regionCells.size();
        int perimeter = 0;

        int rows = grid.length;
        int cols = grid[0].length;

        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

        for (int[] cell : regionCells) {
            int x = cell[0], y = cell[1];
            for (int[] dir : directions) {
                int nx = x + dir[0], ny = y + dir[1];
                if (nx < 0 || nx >= rows || ny < 0 || ny >= cols || grid[nx][ny] != grid[x][y]) {
                    perimeter++;
                }
            }
        }
        return new int[]{area, perimeter};
    }

    public static int calculateTotalPrice(char[][] grid) {
        int totalPrice = 0;
        int rows = grid.length;
        int cols = grid[0].length;

        boolean[][] visited = new boolean[rows][cols];

        for (int x = 0; x < rows; x++) {
            for (int y = 0; y < cols; y++) {
                if (!visited[x][y]) {
                    List<int[]> regionCells = floodFill(grid, x, y, visited);
                    int[] areaAndPerimeter = calculateAreaAndPerimeter(grid, regionCells);
                    int area = areaAndPerimeter[0];
                    int perimeter = areaAndPerimeter[1];
                    totalPrice += area * perimeter;
                }
            }
        }
        return totalPrice;
    }

    public static void main(String[] args) {
        String inputFileName = "input.txt";
        try {
            char[][] grid = parseInput(inputFileName);
            System.out.println(calculateTotalPrice(grid));
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }
}