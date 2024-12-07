class WordFinder
  attr_reader :grid, :center, :target

  def initialize(word, grid)
    @grid = grid
    @center = word[1]
    @target = [word[0], word[2]].sort
  end

  def num_rows
    grid[0].size
  end

  def num_columns
    grid.size
  end

  def count
    num_rows.times.sum do |x|
      num_columns.times.sum do |y|
        count_from_pos(x, y)
      end
    end
  end

  def count_from_pos(x, y)
    return 0 unless grid[y][x] == center
    return 0 if x < 1 || x >= num_rows - 1
    return 0 if y < 1 || y >= num_columns - 1

    diagonals = [
      [grid[y + 1][x + 1], grid[y - 1][x - 1]].sort,
      [grid[y + 1][x - 1], grid[y - 1][x + 1]].sort,
    ]

    if diagonals.count(target) == 2
      1
    else
      0
    end
  end
end

lines = File.readlines('input.txt', chomp: true)
grid = lines.map { _1.split('') }
puts WordFinder.new('MAS', grid).count
