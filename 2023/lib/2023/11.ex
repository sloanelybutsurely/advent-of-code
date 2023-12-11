import AOC
import AOCHelpers

aoc 2023, 11 do
  def p1(input) do
    input
    |> read_input()
    |> calculate_distances()
  end

  def p2(input) do
    input
    |> read_input()
    |> calculate_distances(1_000_000)
  end

  def calculate_distances({galaxies, empty_columns, empty_rows}, expansion \\ 2) do
    distance = make_distance_fn(empty_rows, empty_columns, expansion)

    for [a, b] <- combinations(galaxies, 2), reduce: 0 do
      acc -> acc + distance.(a, b)
    end
  end

  def make_distance_fn(expanded_rows, expanded_columns, expansion) do
    fn {x_a, y_a}, {x_b, y_b} ->
      x_dist = abs(x_b - x_a)
      y_dist = abs(y_b - y_a)

      x_range = to_range(x_a, x_b)
      y_range = to_range(y_a, y_b)

      expanded_x = Enum.count(expanded_columns, &(&1 in x_range))
      expanded_y = Enum.count(expanded_rows, &(&1 in y_range))

      x_dist - expanded_x + expanded_x * expansion + (y_dist - expanded_y) +
        expanded_y * expansion
    end
  end

  def read_input(input) do
    {grid, {x_bounds, y_bounds}} = to_grid(input)

    empty_columns =
      for x <- x_bounds, Enum.all?(column(grid, x, y_bounds), is?(".")), do: x

    empty_rows =
      for y <- y_bounds, Enum.all?(row(grid, y, x_bounds), is?(".")), do: y

    galaxies =
      grid
      |> Enum.filter(&match?({_, "#"}, &1))
      |> Enum.map(&elem(&1, 0))

    {galaxies, empty_columns, empty_rows}
  end

  def row(grid, y, x_bounds) do
    for x <- x_bounds, do: Map.get(grid, {x, y})
  end

  def column(grid, x, y_bounds) do
    for y <- y_bounds, do: Map.get(grid, {x, y})
  end

  def to_range(l, r) when l < r, do: Range.new(l, r)
  def to_range(l, r), do: Range.new(r, l)
end
