import AOC

aoc 2021, 22 do
  def p1 do
    input_cuboids()
    |> Enum.filter(&within_range(-50..50, &1))
    |> Enum.reduce(MapSet.new(), fn
      {:on, ranges}, acc -> MapSet.union(acc, make_set(ranges))
      {:off, ranges}, acc -> MapSet.difference(acc, make_set(ranges))
    end)
    |> MapSet.size()
  end

  def p2 do
  end

  def make_set({xs, ys, zs}) do
    for x <- xs, y <- ys, z <- zs, into: MapSet.new(), do: {x, y, z}
  end

  def within_range(bounds, {tag, ranges}) when tag in ~w[on off]a, do: within_range(bounds, ranges)
  def within_range(bounds, {x, y, z}), do:
    within_range(bounds, x) and within_range(bounds, y) and within_range(bounds, z)
  def within_range(bs..be, s..e), do: bs <= s and e <= be

  def parse_cuboid_ranges(str) do
    ~r/x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
      |> Regex.run(str, capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(2)
      |> Enum.map(&apply(Range, :new, &1))
      |> List.to_tuple()
  end

  def input_cuboids() do
    input_stream()
    |> Enum.map(fn
      "on " <> rest -> {:on, parse_cuboid_ranges(rest)}
      "off " <> rest -> {:off, parse_cuboid_ranges(rest)}
    end)
  end
end
