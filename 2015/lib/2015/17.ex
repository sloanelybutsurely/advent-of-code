import AOC

aoc 2015, 17 do
  def input() do
    input_stream()
    |> Stream.map(&String.to_integer/1)
    |> Enum.sort(:desc)
  end

  def p1 do
    input()
    |> combinations()
    |> Enum.filter(fn comb -> Enum.sum(comb) == 150 end)
    |> length()
  end

  def p2 do
    working_combinations =
      input()
      |> combinations()
      |> Enum.filter(fn comb -> Enum.sum(comb) == 150 end)
      |> Enum.sort_by(&length/1)

    size_of_fewest_container_combination = working_combinations |> hd() |> length()

    working_combinations
    |> Enum.count(&(length(&1) == size_of_fewest_container_combination))
  end

  def combinations(values, combination \\ [])
  def combinations([], combination), do: [combination]

  def combinations([x | xs], combination) do
    combinations(xs, [x | combination]) ++ combinations(xs, combination)
  end
end
