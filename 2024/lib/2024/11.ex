import AOC
import AOC.Prelude

aoc 2024, 11 do
  require Integer

  def p1(input) do
    input
    |> read_stones()
    |> count_stones_after_blink(25)
  end

  def p2(input) do
    input
    |> read_stones()
    |> count_stones_after_blink(75)
  end

  ##

  defp count_stones_after_blink(stones, n) do
    stones
    |> Stream.iterate(&blink/1)
    |> Enum.at(n)
    |> count_stones()
  end

  defp blink(stones) do
    for {stone, count} <- stones, reduce: stones do
      stones ->
        cond do
          stone == 0 ->
            stones
            |> remove_stones(0, count)
            |> add_stones(1, count)

          even_digits?(stone) ->
            {left, right} = split_stone(stone)

            stones
            |> remove_stones(stone, count)
            |> add_stones(left, count)
            |> add_stones(right, count)

          true ->
            stones
            |> remove_stones(stone, count)
            |> add_stones(stone * 2024, count)
        end
    end
  end

  defp even_digits?(num) do
    num
    |> Integer.digits()
    |> Enum.count()
    |> Integer.is_even()
  end

  defp split_stone(num) do
    digits = Integer.digits(num)

    {left, right} = Enum.split(digits, div(length(digits), 2))

    {Integer.undigits(left), Integer.undigits(right)}
  end

  defp remove_stones(stones, num, count) do
    Map.update(stones, num, 0, &(&1 - count))
  end

  defp add_stones(stones, num, count) do
    Map.update(stones, num, count, &(&1 + count))
  end

  defp count_stones(stones) do
    stones
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  ## input

  defp read_stones(input) do
    input
    |> ints()
    |> Enum.frequencies()
  end
end
