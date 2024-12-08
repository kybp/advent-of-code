class WordFinder
  attr_reader :word, :grid

  def initialize(word, grid)
    @word = word
    @grid = grid
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
    return 0 unless grid[y][x] == word[0]

    all_directions.sum do |direction|
      find_word(x, y, word[1..-1], direction)
    end
  end

  def find_word(x, y, target, directions)
    directions.each do |direction|
      case direction
      when :left; x -= 1
      when :right; x += 1
      when :up; y -= 1
      when :down; y += 1
      end
    end

    return 1 if target.empty?
    return 0 if x < 0 || x >= num_rows
    return 0 if y < 0 || y >= num_columns
    return 0 if grid[y][x] != target[0]

    find_word(x, y, target[1..-1], directions)
  end

  def all_directions
    [
      %i[up],
      %i[left],
      %i[down],
      %i[right],
      %i[up left],
      %i[up right],
      %i[down left],
      %i[down right],
    ]
  end
end

lines = File.readlines('2024/input/day-04.txt', chomp: true)
grid = lines.map { _1.split('') }
puts WordFinder.new('XMAS', grid).count
