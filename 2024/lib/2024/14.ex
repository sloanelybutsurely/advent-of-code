import AOC

aoc 2024, 14 do
  # @bounds {11, 7}
  @bounds {101, 103}

  def p1(input) do
    input
    |> read_robots()
    |> Stream.iterate(fn robots ->
      Enum.map(robots, &move_robot(&1, @bounds))
    end)
    |> Enum.at(100)
    |> Enum.map(&elem(&1, 0))
    |> Enum.frequencies_by(&quadrant(&1, @bounds))
    |> Map.delete(-1)
    |> Map.values()
    |> Enum.product()
  end

  def p2(input) do
    input
    |> read_robots()
    |> Stream.iterate(fn robots ->
      Enum.map(robots, &move_robot(&1, @bounds))
    end)
    |> Stream.with_index()
    # noticed convergence every 101 seconds
    |> Stream.take_every(101)
    |> Enum.each(fn {robots, n} ->
      IO.puts("")
      IO.puts("------------------ Seconds elapsed: #{n} ------------------")
      IO.puts("")

      robots
      |> Enum.frequencies_by(&elem(&1, 0))
      |> render(@bounds)

      IO.gets("Continue?")
    end)
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

  defp quadrant({x, y}, {max_x, max_y}) do
    half_x = div(max_x, 2)
    half_y = div(max_y, 2)

    case {x < half_x, y < half_y, half_x == x or half_y == y} do
      # special grouping on the lines bisecting that plane
      {_, _, true} -> -1
      # bin into quadrants
      {true, true, _} -> 0
      {false, true, _} -> 1
      {true, false, _} -> 2
      {false, false, _} -> 3
    end
  end

  defp render(robots, {max_x, max_y}) do
    for y <- 0..max_y do
      line =
        for x <- 0..max_x, into: "" do
          "#{Map.get(robots, {x, y}, " ")}"
        end

      IO.puts(line)
    end

    robots
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
