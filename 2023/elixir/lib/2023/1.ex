import AOC

aoc 2023, 1 do
  def p1(input) do
    input
    |> String.split("\n")
    |> Stream.map(&get_first_and_last_numbers_as_number/1)
    |> Enum.sum()
  end

  def p2(_input) do
  end

  def get_first_and_last_numbers_as_number(str) do
    trimmed =
      str
      |> String.replace(~r/^[^\d]+/, "")
      |> String.reverse()
      |> String.replace(~r/^[^\d]+/, "")
      |> String.reverse()

    first = String.first(trimmed)
    last = String.last(trimmed)

    String.to_integer(first <> last)
  end
end
