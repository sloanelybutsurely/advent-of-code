import AOC

aoc 2024, 4 do
  @directions ~w[n ne e se s sw w nw]a

  def p1(input) do
    grid = to_grid(input)
    {xs, ys} = bounds(grid)

    pos_and_dirs = for x <- xs, y <- ys, dir <- @directions, do: {{x, y}, dir}

    Enum.count(pos_and_dirs, fn {pos, dir} ->
      spells_xmas?(grid, pos, dir)
    end)
  end

  def p2(input) do
    grid = to_grid(input)
    {xl..xh, yl..yh} = bounds(grid)

    xs = (xl + 1)..(xh - 1)
    ys = (yl + 1)..(yh - 1)

    positions = for x <- xs, y <- ys, do: {x, y}

    Enum.count(positions, &x_mas?(grid, &1))
  end

  def spells_xmas?(grid, pos, dir) do
    spells?(grid, pos, dir, "XMAS")
  end

  def x_mas?(grid, pos) do
    ~w[ne se sw nw]a
    |> Enum.map(&{&1, move(pos, &1)})
    |> Enum.count(fn {op_dir, pos} ->
      dir =
        case op_dir do
          :ne -> :sw
          :nw -> :se
          :se -> :nw
          :sw -> :ne
        end

      spells?(grid, pos, dir, "MAS")
    end) == 2
  end

  def spells?(grid, pos, dir, word) do
    letters = String.split(word, "", trim: true)

    grid
    |> walk(pos, dir)
    |> Stream.zip_with(letters, &(&1 == &2))
    |> Enum.all?()
  end

  defp to_grid(input) do
    grid =
      input
      |> String.split("\n")
      |> Enum.map(fn line ->
        line
        |> String.split("", trim: true)
        |> List.to_tuple()
      end)
      |> List.to_tuple()

    grid
  end

  def bounds(grid) do
    ys = 0..(tuple_size(grid) - 1)
    xs = 0..(tuple_size(elem(grid, 0)) - 1)

    {xs, ys}
  end

  def walk(grid, pos, dir) do
    pos
    |> indexes(dir)
    |> Stream.map(&get(grid, &1))
  end

  def indexes(pos, dir) do
    Stream.iterate(pos, &move(&1, dir))
  end

  def move({x, y}, :n), do: {x, y - 1}
  def move({x, y}, :e), do: {x + 1, y}
  def move({x, y}, :s), do: {x, y + 1}
  def move({x, y}, :w), do: {x - 1, y}
  def move(pos, :ne), do: pos |> move(:n) |> move(:e)
  def move(pos, :se), do: pos |> move(:s) |> move(:e)
  def move(pos, :sw), do: pos |> move(:s) |> move(:w)
  def move(pos, :nw), do: pos |> move(:n) |> move(:w)

  def get(grid, {x, y}) do
    {xs, ys} = bounds(grid)

    if x in xs and y in ys do
      elem(elem(grid, y), x)
    end
  end
end
