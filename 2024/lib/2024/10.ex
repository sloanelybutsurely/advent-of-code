import AOC
import AOC.Prelude

aoc 2024, 10 do
  def p1(input) do
    map = read_map(input)

    trailheads = map |> Map.filter(&match?({_, 0}, &1)) |> Map.keys()

    trailheads
    |> Enum.map(&score(&1, map))
    |> Enum.sum()
  end

  def p2(input) do
    map = read_map(input)

    trailheads = map |> Map.filter(&match?({_, 0}, &1)) |> Map.keys()

    trailheads
    |> Enum.map(&rating(&1, map))
    |> Enum.sum()
  end

  ## part 1

  defp score(pos, map) do
    pos
    |> reachable_nines(map, MapSet.new())
    |> Enum.count()
  end

  defp reachable_nines(pos, map, seen) do
    seen = MapSet.put(seen, pos)
    curr = Map.get(map, pos)

    if curr == 9 do
      [pos]
    else
      [&n/1, &s/1, &e/1, &w/1]
      |> Enum.map(& &1.(pos))
      |> Enum.reject(&MapSet.member?(seen, &1))
      |> Enum.filter(&(Map.get(map, &1, 0) == curr + 1))
      |> Enum.flat_map(&reachable_nines(&1, map, seen))
      |> Enum.uniq()
    end
  end

  ## part 2

  defp rating(pos, map, seen \\ MapSet.new()) do
    seen = MapSet.put(seen, pos)
    curr = Map.get(map, pos)

    if curr == 9 do
      1
    else
      [&n/1, &s/1, &e/1, &w/1]
      |> Enum.map(& &1.(pos))
      |> Enum.reject(&MapSet.member?(seen, &1))
      |> Enum.filter(&(Map.get(map, &1, 0) == curr + 1))
      |> Enum.map(&rating(&1, map, seen))
      |> Enum.sum()
    end
  end

  ## input

  def read_map(input) do
    input
    |> map_grid()
    |> Enum.reject(&match?({_, "."}, &1))
    |> Map.new(fn
      {k, v} -> {k, String.to_integer(v)}
    end)
  end
end
