import AOC

aoc 2024, 1 do
  def p1(input) do
    {left, right} = read_input(input)

    left
    |> Enum.sort()
    |> Enum.zip_with(Enum.sort(right), &abs(&1 - &2))
    |> Enum.sum()
  end

  def p2(input) do
    {left, right} = read_input(input)

    right_list_freqs = Enum.frequencies(right)

    left
    |> Enum.map(&(&1 * Map.get(right_list_freqs, &1, 0)))
    |> Enum.sum()
  end

  defp read_input(input) do
    input
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
    |> Enum.unzip()
  end
end
