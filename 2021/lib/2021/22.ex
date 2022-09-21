import AOC

aoc 2021, 22 do

  def p1 do
    input_cuboids()
    |> Enum.filter(&within_range(-50..50, &1))
    |> calculate_final_lights_on()
  end

  def p2 do
    input_cuboids()
    |> calculate_final_lights_on()
  end

  def calculate_final_lights_on(inp) do
    {ons, offs} = Enum.reduce(inp, {[], []}, fn
      {op, cuboid}, {seen, intersections} ->
        IO.puts("")
        IO.puts("--------------")
        IO.puts("")
        IO.inspect(cuboid)
        intersections =
          Enum.concat([
            intersections,
            seen
            |> Enum.filter(&intersect?(&1, cuboid))
            |> IO.inspect()
            |> Enum.map(fn intersected ->
              intersected
              |> intersection(cuboid)
            end)
            |> IO.inspect()
          ])

        seen = if op == :on, do: [cuboid | seen], else: seen

        {seen, intersections}
    end)

    {sum_volumes(ons), sum_volumes(offs)}
  end

  def sum_volumes(ranges), do:
    ranges
    |> Enum.map(&volume/1)
    |> Enum.sum()

  def within_range(bounds, {tag, ranges}) when tag in ~w[on off]a, do: within_range(bounds, ranges)
  def within_range(bounds, {x, y, z}), do:
    [x, y, z] |> Enum.all?(&within_range(bounds, &1))
  def within_range(bs..be, s..e), do: bs <= s and e <= be

  def intersect?({a1, b1, c1}, {a2, b2, c2}), do:
    intersect?(a1, a2) and intersect?(b1, b2) and intersect?(c1, c2)
  def intersect?(a, b), do: not Range.disjoint?(a, b)

  def intersection({min_x_a..max_x_a, min_y_a..max_y_a, min_z_a..max_z_a}, {min_x_b..max_x_b, min_y_b..max_y_b, min_z_b..max_z_b}) do
    {(max(min_x_a, min_x_b))..min(max_x_a, max_x_b), max(min_y_a, min_y_b)..min(max_y_a, max_y_b), max(min_z_a, min_z_b)..min(max_z_a, max_z_b)}
  end

  def volume(bounds), do:
    bounds |> Tuple.to_list() |> Enum.map(&Range.size/1) |> Enum.product()

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
