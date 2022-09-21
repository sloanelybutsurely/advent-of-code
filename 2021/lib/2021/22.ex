import AOC

aoc 2021, 22 do
  alias :rstar, as: RStar
  alias :rstar_geometry, as: RStarGeometry

  def p1 do
    input_cuboids()
    |> Enum.filter(&within_range(-50..50, &1))
    |> Enum.reduce(MapSet.new(), fn
      {:on, ranges}, acc -> MapSet.union(acc, make_set(ranges))
      {:off, ranges}, acc -> MapSet.difference(acc, make_set(ranges))
    end)
    |> MapSet.size()
  end

  def make_set({xs, ys, zs}) do
    for x <- xs, y <- ys, z <- zs, into: MapSet.new(), do: {x, y, z}
  end

  def within_range(bounds, {tag, ranges}) when tag in ~w[on off]a, do: within_range(bounds, ranges)
  def within_range(bounds, {x, y, z}), do:
    [x, y, z] |> Enum.all?(&within_range(bounds, &1))
  def within_range(bs..be, s..e), do: bs <= s and e <= be

  def p2 do
    input_cuboids()
    |> Enum.reduce(RStar.new(3), fn
      {:on, {x1..x2, y1..y2, z1..z2}}, tree ->
        cuboid = RStarGeometry.new(3, [{x1, x2}, {y1, y2}, {z1, z2}], nil)

        IO.inspect(cuboid)
        RStar.search_within(tree, cuboid)
        |> Enum.each(fn geo ->
          IO.write(" ... ")
          IO.inspect(RStarGeometry.intersect(cuboid, geo))
        end)

        IO.gets("")

        RStar.insert(tree, cuboid)
      _, tree -> tree
    end)
  end

  def intersect?({a1, b1, c1}, {a2, b2, c2}), do:
    intersect?(a1, a2) or intersect?(b1, b2) or intersect?(c1, c2)
  def intersect?(a, b), do: not Range.disjoint?(a, b)

  def parse_cuboid_ranges(str) do
    ~r/x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/
      |> Regex.run(str, capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(2)
      |> Enum.map(&Enum.sort/1)
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
