import AOC

aoc 2021, 1 do
  def input_list,
    do:
      input_stream()
      |> Stream.map(&String.to_integer/1)
      |> Enum.to_list()

  def p1 do
    input_list = input_list()
    input_list_ = Enum.drop(input_list, 1)

    Enum.zip_reduce(
      input_list,
      input_list_,
      0,
      &if(&1 < &2, do: &3 + 1, else: &3)
    )
  end

  def p2 do
    input_list()
    |> window_increases()
  end

  defp window_increases(list, acc \\ 0)

  defp window_increases([a, b, c, d | _] = list, acc) do
    acc = if a + b + c < b + c + d, do: acc + 1, else: acc
    window_increases(Enum.drop(list, 1), acc)
  end

  defp window_increases(_, acc), do: acc
end
