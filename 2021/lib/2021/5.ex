import AOC

aoc 2021, 5 do
  import NimbleParsec

  defparsec(
    :vent_readout_parsec,
    integer(min: 1)
    |> ignore(string(","))
    |> integer(min: 1)
    |> ignore(string(" -> "))
    |> integer(min: 1)
    |> ignore(string(","))
    |> integer(min: 1)
  )

  def parse_vent_readout(line) do
    {:ok, [x1, y1, x2, y2], _, _, _, _} = line |> vent_readout_parsec()

    {{x1, y1}, {x2, y2}}
  end

  def input_stream(), do: super() |> Stream.map(&parse_vent_readout/1)

  def p1 do
    input_stream()
    |> Enum.reduce(Map.new(), &record_line_on_map(&1, &2, false))
    |> Map.values()
    |> Enum.count(&(&1 > 1))
  end

  def p2 do
    input_stream()
    |> Enum.reduce(Map.new(), &record_line_on_map(&1, &2, true))
    |> Map.values()
    |> Enum.count(&(&1 > 1))
  end

  def record_line_on_map(line, map, include_diagonals),
    do:
      map
      |> record_points_on_map(points_on_line(line, include_diagonals))

  def record_points_on_map(map, points) do
    for point <- points, reduce: map do
      map -> Map.update(map, point, 1, &(&1 + 1))
    end
  end

  # vertical lines
  def points_on_line({{x, y1}, {x, y2}}, _) do
    for y <- y1..y2, do: {x, y}
  end

  # horizontal lines
  def points_on_line({{x1, y}, {x2, y}}, _) do
    for x <- x1..x2, do: {x, y}
  end

  # diagonal lines
  def points_on_line({{x1, y1}, {x2, y2}}, true) do
    Enum.zip(x1..x2, y1..y2)
  end

  def points_on_line(_, false), do: []
end
