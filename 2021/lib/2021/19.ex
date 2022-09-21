import AOC

aoc 2021, 19 do
  def p1 do
    scanners = input()

    align_scanners(scanners)
    |> MapSet.size()
  end

  def p2 do
  end

  def attempt_alignment(source, canidate) do
    found =
      source
      |> potential_transforms(canidate)
      |> Enum.map(fn transform ->
        apply_transform(canidate, transform)
      end)
      |> Enum.find(:miss, fn transformed ->

        overlap(source, transformed) >= 12
      end)

    case found do
      :miss -> :miss
      transformed -> {:ok, transformed}
    end
  end

  def overlap(a, b), do: MapSet.intersection(a, b) |> MapSet.size()

  def union(map_sets), do: Enum.reduce(map_sets, MapSet.new(), &MapSet.union/2)

  def align_scanners([start | rest]) do
    align_scanners(rest, start)
  end

  def align_scanners(scanners, aligned, skipped \\ [])
  def align_scanners([], aligned, []), do: aligned
  def align_scanners([], aligned, skipped) do
    IO.puts("")
    align_scanners([aligned | skipped])
  end
  def align_scanners([canidate | rest], aligned, skipped) do
    case aligned |> attempt_alignment(canidate) do
      :miss -> 
        IO.write(".")
        align_scanners(rest, aligned, [canidate | skipped])
      {:ok, transformed} ->
        IO.write("+")
        align_scanners(rest, MapSet.union(transformed, aligned), skipped)
    end
  end

  def potential_transforms(set_a, set_b) do
    for point_a <- set_a, point_b <- set_b, rotation <- rotations() do
      make_transform_fun(point_a, point_b, rotation)
    end
  end

  def make_transform_fun(point_a, point_b, rotation) do
    translation_fun =
      make_translation_fun(point_a, apply_rotation(point_b, rotation))
    fn point ->
      point
      |> apply_rotation(rotation)
      |> apply_translation(translation_fun)
    end
  end

  def apply_transform(%MapSet{} = set, fun) do
    for point <- set, into: MapSet.new(), do: fun.(point)
  end

  def make_translation_fun({x0, y0, z0}, {x1, y1, z1}) do
    {
      make_translation_fun(x0, x1),
      make_translation_fun(y0, y1),
      make_translation_fun(z0, z1)
    }
  end

  def make_translation_fun(a, b) do
    fn c -> c + (a - b) end
  end

  def apply_translation({x, y, z}, {tx, ty, tz}) do
    {tx.(x), ty.(y), tz.(z)}
  end

  def rotations() do
    for a <- [:x, :y, :z], b <- [:x, :y, :z] -- [a], c <- [:x, :y, :z] -- [a, b], s_a <- [1, -1], s_b <- [1, -1], s_c <- [1, -1] do
      {{s_a, a}, {s_b, b}, {s_c, c}}
    end
  end

  def apply_rotation({x, y, z}, {{s_a, a}, {s_b, b}, {s_c, c}}) do
    map = %{ x: x, y: y, z: z}

    {s_a * Map.get(map, a), s_b * Map.get(map, b), s_c * Map.get(map, c)}
  end


  def input(input_string \\ input_string()) do
    input_string
    |> String.split("\n\n")
    |> Enum.map(fn scanner_str ->
      scanner_str
      |> String.split("\n", trim: true)
      |> Enum.drop(1)
      |> Enum.map(fn line ->
        line
        |> String.split(",", trim: true)
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
      end)
      |> Enum.into(MapSet.new())
    end)
  end
end
