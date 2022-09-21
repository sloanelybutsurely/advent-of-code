import AOC

aoc 2021, 9 do
  def input_map(),
    do:
      input_string()
      |> String.split("\n")
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, i} ->
        line
        |> String.split("", trim: true)
        |> Enum.map(&String.to_integer/1)
        |> Enum.with_index()
        |> Enum.map(fn {cell, j} -> {{i, j}, cell} end)
      end)
      |> Map.new()

  def n({i, j}), do: {i - 1, j}
  def e({i, j}), do: {i, j + 1}
  def s({i, j}), do: {i + 1, j}
  def w({i, j}), do: {i, j - 1}

  def at(map, pos), do: Map.get(map, pos, :infinity)

  def directions(), do: [&n/1, &e/1, &s/1, &w/1]

  def neighbors(map, origin) do
    for d <- directions(), pos = d.(origin) do
      {pos, at(map, pos)}
    end
  end

  def gt({_, a}, b), do: a > b

  def basin_from(map, pos, seen \\ MapSet.new()) do
    curr = at(map, pos)

    if min(curr, 9) == 9 do
      MapSet.new()
    else
      seen = MapSet.put(seen, pos)

      inclines =
        neighbors(map, pos)
        |> Enum.filter(&gt(&1, curr))
        |> Enum.map(&elem(&1, 0))

      for inc <- inclines, reduce: seen do
        seen ->
          if MapSet.member?(seen, inc) do
            seen
          else
            MapSet.union(seen, basin_from(map, inc, seen))
          end
      end
    end
  end

  def p1 do
    map = input_map()

    for i <- 0..99, j <- 0..99 do
      pos = {i, j}
      curr = at(map, pos)

      neighbors =
        directions()
        |> Enum.map(fn d -> at(map, d.(pos)) end)

      if Enum.all?(neighbors, &(curr < &1)) do
        curr + 1
      else
        0
      end
    end
    |> Enum.sum()
  end

  def p2 do
    map = input_map()

    for i <- 0..99, j <- 0..99, pos = {i, j} do
      basin_from(map, pos) |> MapSet.size()
    end
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.product()
  end
end
