import AOC

aoc 2024, 2 do
  def p1(input) do
    input
    |> list_of_lists_of_numbers()
    |> Enum.count(&safe?/1)
  end

  def p2(input) do
    input
    |> list_of_lists_of_numbers()
    |> Enum.count(&safe_with_dampener?(&1))
  end

  defp list_of_lists_of_numbers(input) do
    input
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  defp safe?(report) do
    diffs = differences(report)

    Enum.all?(diffs, &(1 <= &1 and &1 <= 3)) or
      Enum.all?(diffs, &(-3 <= &1 and &1 <= -1))
  end

  def safe_with_dampener?(report) do
    reports_with_a_single_level_removed =
      for i <- 0..(length(report) - 1) do
        {head, [_ | tail]} = Enum.split(report, i)
        head ++ tail
      end

    safe?(report) or
      Enum.any?(reports_with_a_single_level_removed, &safe?/1)
  end

  defp differences([_ | rest] = list) do
    Enum.zip_with(list, rest, &(&1 - &2))
  end
end
