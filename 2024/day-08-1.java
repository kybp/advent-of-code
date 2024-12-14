import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

public class AntinodeCounter {
    public static void main(String[] args) throws IOException {
        Stream<String> lines = Files.lines(Path.of("2024/input/day-08.txt"));
        AntinodeCounter counter = new AntinodeCounter(lines);
        System.out.println(counter.countAntinodes());
    }

    private Map<Point, Character> pointContent;
    private Map<Character, List<Point>> antennas;
    private int width;
    private int height;

    public AntinodeCounter(Stream<String> lines) {
        pointContent = new HashMap<>();
        antennas = new HashMap<>();

         // Mutable wrapper
        int[] row = {0};

        lines.forEach(line -> {
                for (int column = 0; column < line.length(); ++column) {
                    Point point = new Point(row[0], column);
                    char content = line.charAt(column);

                    pointContent.put(point, content);

                    if (isAntenna(content)) {
                        antennas.putIfAbsent(content, new ArrayList<>());
                        antennas.get(content).add(point);
                    }
                }

                ++row[0];
            });

        width = height = row[0];
    }

    public int countAntinodes() {
        Set<Point> antinodes = new HashSet<>();

        for (char frequency : antennas.keySet()) {
            for (List<Point> antennaPair : antennaPairs(frequency)) {
                int dx = antennaPair.get(1).getX() - antennaPair.get(0).getX();
                int dy = antennaPair.get(1).getY() - antennaPair.get(0).getY();

                Point[] newAntinodes = {
                    new Point(antennaPair.get(0).getX() - dx, antennaPair.get(0).getY() - dy),
                    new Point(antennaPair.get(1).getX() + dx, antennaPair.get(1).getY() + dy),
                };

                for (Point antinode : newAntinodes) {
                    if (isInBounds(antinode)) antinodes.add(antinode);
                }
            }
        }

        return antinodes.size();
    }

    private boolean isAntenna(char content) {
        return content != '.';
    }

    private boolean isInBounds(Point point) {
        return point.getX() >= 0 && point.getX() < width &&
            point.getY() >= 0 && point.getY() < height;
    }

    private List<List<Point>> antennaPairs(char frequency) {
        List<Point> antennasWithFrequency = antennas.get(frequency);
        List<List<Point>> result = new ArrayList<>();

        for (Point pointA : antennasWithFrequency) {
            for (Point pointB : antennasWithFrequency) {
                if (pointA.equals(pointB)) continue;
                result.add(Arrays.asList(pointA, pointB));
            }
        }

        return result;
    }
}

class Point {
    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Point point = (Point) obj;
        return x == point.x && y == point.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}
