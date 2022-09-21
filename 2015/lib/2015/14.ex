import AOC

aoc 2015, 14 do
  @end_time 2503

  def parse_line(line) do
    Regex.run(
      ~r/^\w+ can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds.$/,
      line,
      capture: :all_but_first
    )
    |> Enum.map(&String.to_integer/1)
  end

  def distance_traveled(speed, run, rest, time) do
    full_cycle = run + rest
    full_cycles = div(time, full_cycle)

    time_into_current_cycle = rem(time, run + rest)

    total_running_time = full_cycles * run + min(time_into_current_cycle, run)

    total_running_time * speed
  end

  def p1 do
    input_stream()
    |> Stream.map(&parse_line/1)
    |> Stream.map(fn [speed, run, rest] -> distance_traveled(speed, run, rest, @end_time) end)
    |> Enum.max()
  end

  def p2 do
    reindeer =
      input_stream()
      |> Enum.map(&parse_line/1)
      |> Enum.map(&{0, &1})

    for s <- 1..@end_time, reduce: reindeer do
      reindeer ->
        winning_distance =
          reindeer
          |> Enum.map(fn {_, [speed, run, rest]} -> distance_traveled(speed, run, rest, s) end)
          |> Enum.max()

        reindeer
        |> Enum.map(fn {score, [speed, run, rest] = properties} ->
          if distance_traveled(speed, run, rest, s) == winning_distance do
            {score + 1, properties}
          else
            {score, properties}
          end
        end)
    end
    |> Enum.map(&elem(&1, 0))
    |> Enum.max()
  end
end
