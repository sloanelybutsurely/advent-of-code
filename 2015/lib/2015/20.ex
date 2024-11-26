import AOC

aoc 2015, 20 do
  def p1 do
    goal = input()

    Stream.iterate(1, &(&1 + 1))
    |> Enum.find(fn house ->
      gifts_for_house_1(house) >= goal
    end)
  end

  def p2 do
    goal = input()

    Stream.iterate(1, &(&1 + 1))
    |> Enum.find(fn house ->
      gifts_for_house_2(house) >= goal
    end)
  end

  def gifts_for_house_1(house) do
    elves =
      house
      |> divisors()
      |> Enum.sum()

    elves * 10
  end

  defp gifts_for_house_2(house) do
    elves =
      house
      |> divisors()
      |> Enum.filter(&(div(house, &1) <= 50))
      |> Enum.sum()

    elves * 11
  end

  defp divisors(n) do
    e = n |> :math.sqrt() |> trunc

    Enum.flat_map(1..e, fn
      x when rem(n, x) != 0 -> []
      x when x != div(n, x) -> [x, div(n, x)]
      x -> [x]
    end)
  end

  defp input do
    input_string()
    |> String.trim()
    |> String.to_integer()
  end
end
