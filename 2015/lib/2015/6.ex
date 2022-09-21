import AOC

aoc 2015, 6 do
  import NimbleParsec

  defparsec(
    :coord,
    integer(min: 1)
    |> ignore(string(","))
    |> integer(min: 1)
    |> label("coordinate pair")
  )

  defparsec(
    :command,
    choice([
      string("turn on"),
      string("turn off"),
      string("toggle")
    ])
    |> ignore(string(" "))
    |> parsec(:coord)
    |> ignore(string(" through "))
    |> parsec(:coord)
  )

  def parse_command(line) do
    command(line)
    |> then(fn {:ok, v, _, _, _, _} -> v end)
    |> then(fn
      ["turn on", x1, y1, x2, y2] -> {:turn_on, {x1, y1}, {x2, y2}}
      ["turn off", x1, y1, x2, y2] -> {:turn_off, {x1, y1}, {x2, y2}}
      ["toggle", x1, y1, x2, y2] -> {:toggle, {x1, y1}, {x2, y2}}
    end)
  end

  def command_stream(), do: input_stream() |> Stream.map(&parse_command/1)

  def p1 do
    lights = for x <- 0..999, y <- 0..999, into: %{}, do: {{x, y}, 0}

    command_stream()
    |> Enum.reduce(lights, fn
      {:turn_on, from, to}, lights ->
        set_range(lights, from, to, 1)

      {:turn_off, from, to}, lights ->
        set_range(lights, from, to, 0)

      {:toggle, from, to}, lights ->
        update_range(lights, from, to, fn
          0 -> 1
          1 -> 0
        end)
    end)
    |> Map.values()
    |> Enum.sum()
  end

  def p2 do
    lights = for x <- 0..999, y <- 0..999, into: %{}, do: {{x, y}, 0}

    command_stream()
    |> Enum.reduce(lights, fn
      {:turn_on, from, to}, lights -> update_range(lights, from, to, &(&1 + 1))
      {:turn_off, from, to}, lights -> update_range(lights, from, to, &max(0, &1 - 1))
      {:toggle, from, to}, lights -> update_range(lights, from, to, &(&1 + 2))
    end)
    |> Map.values()
    |> Enum.sum()
  end

  def set_range(map, from, to, value), do: update_range(map, from, to, always(value))

  def update_range(map, {x1, y1}, {x2, y2}, fun) do
    for x <- x1..x2, y <- y1..y2, reduce: map do
      map -> Map.update!(map, {x, y}, fun)
    end
  end

  def always(v), do: fn _ -> v end
end
