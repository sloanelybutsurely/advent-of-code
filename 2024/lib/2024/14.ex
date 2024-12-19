import AOC

aoc 2024, 14 do
  @example_bounds
  # @bounds @example_bounds
  # @real_bounds {101, 103}

  def p1(input) do
    robots =
      input
      |> read_robots()
      |> Stream.iterate(fn robots ->
        Enum.map(robots, &move_robot(&1, @bounds))
      end)
      |> Enum.at(100)
      |> Enum.frequencies_by(&elem(&1, 0))

    render(robots, @bounds)

    :ok
  end

  def p2(_input) do
  end

  defp move_robot({{x, y}, {vx, vy} = velocity}, {max_x, max_y}) do
    x_next = wrap_value(x + vx, max_x)
    y_next = wrap_value(y + vy, max_y)

    {{x_next, y_next}, velocity}
  end

  defp wrap_value(v, max) do
    if v < 0 do
      max + v
    else
      rem(v, max)
    end
  end

  defp render(robots, {max_x, max_y}) do
    for y <- 0..max_y do
      line =
        for x <- 0..max_x, into: "" do
          "#{Map.get(robots, {x, y}, ".")}"
        end

      IO.puts(line)
    end
  end

  ## input

  defp read_robots(input) do
    ~r/p=(\d+),(\d+) v=(-?\d+),(-?\d+)/
    |> Regex.scan(input, capture: :all_but_first)
    |> Enum.map(fn nums ->
      [px, py, vx, vy] = Enum.map(nums, &String.to_integer/1)
      {{px, py}, {vx, vy}}
    end)
  end
end
