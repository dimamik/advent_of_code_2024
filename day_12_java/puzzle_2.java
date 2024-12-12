import java.io.*;
import java.util.*;

public class ZoneCalculator {
    public static void main(String[] args) throws IOException {
        String filename = "input.txt";

        List<String> grid = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null && !line.strip().isEmpty()) {
                grid.add(line.strip());
            }
        }

        int gridWidth = grid.get(0).length();
        int gridHeight = grid.size();

        Set<Point> checked = new HashSet<>();
        int total = 0;

        for (int y = 0; y < gridHeight; y++) {
            for (int x = 0; x < gridWidth; x++) {
                Point start = new Point(x, y);
                if (checked.contains(start)) continue;
                Set<Point> zone = buildZone(start, grid);
                int area = zone.size();
                int corners = calculateZoneCorners(zone);
                total += area * corners;
                checked.addAll(zone); // Union all elements
            }
        }
        System.out.println(total);
    }

    private static Set<Point> buildZone(Point start, List<String> grid) {
        int gridWidth = grid.get(0).length();
        int gridHeight = grid.size();
        Deque<Point> queue = new ArrayDeque<>();
        queue.add(new Point(start.x + 1, start.y));
        queue.add(new Point(start.x - 1, start.y));
        queue.add(new Point(start.x, start.y + 1));
        queue.add(new Point(start.x, start.y - 1));
        char v = grid.get(start.y).charAt(start.x);
        Set<Point> zone = new HashSet<>();
        zone.add(start);

        while (!queue.isEmpty()) {
            Point curr = queue.pollFirst();
            if (zone.contains(curr)) continue;
            if (curr.x < 0 || curr.x >= gridWidth) continue;
            if (curr.y < 0 || curr.y >= gridHeight) continue;
            if (grid.get(curr.y).charAt(curr.x) != v) continue;

            zone.add(curr);
            queue.add(new Point(curr.x + 1, curr.y));
            queue.add(new Point(curr.x - 1, curr.y));
            queue.add(new Point(curr.x, curr.y + 1));
            queue.add(new Point(curr.x, curr.y - 1));
        }
        return zone;
    }

    private static int calculateZoneCorners(Set<Point> zone) {
        int corners = 0;
        for (Point t : zone) {
            boolean N = zone.contains(new Point(t.x, t.y - 1));
            boolean E = zone.contains(new Point(t.x + 1, t.y));
            boolean S = zone.contains(new Point(t.x, t.y + 1));
            boolean W = zone.contains(new Point(t.x - 1, t.y));
            boolean NE = zone.contains(new Point(t.x + 1, t.y - 1));
            boolean SE = zone.contains(new Point(t.x + 1, t.y + 1));
            boolean NW = zone.contains(new Point(t.x - 1, t.y - 1));
            boolean SW = zone.contains(new Point(t.x - 1, t.y + 1));

            if (!N && !E && !S && !W) corners += 4;

            if (N && !E && !S && !W) corners += 2;
            if (E && !S && !W && !N) corners += 2;
            if (S && !W && !N && !E) corners += 2;
            if (W && !N && !E && !S) corners += 2;

            if (S && E && !N && !W) corners += 1;
            if (S && W && !N && !E) corners += 1;
            if (N && E && !S && !W) corners += 1;
            if (N && W && !S && !E) corners += 1;

            if (E && N && !NE) corners += 1;
            if (E && S && !SE) corners += 1;
            if (W && N && !NW) corners += 1;
            if (W && S && !SW) corners += 1;
        }
        return corners;
    }

    private static class Point {
        int x, y;

        Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }
}