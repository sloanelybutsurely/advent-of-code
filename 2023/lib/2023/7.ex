import AOC
import AOCHelpers

aoc 2023, 7 do
  def p1(input) do
    input
    |> read_input()
    |> Enum.map(fn [hand, bet] ->
      counts =
        hand
        |> Enum.frequencies()
        |> Map.values()
        |> Enum.sort(:desc)

      [counts, hand, bet]
    end)
    |> Enum.sort(&compare_hands_1/2)
    |> Enum.map(&Enum.at(&1, 2))
    |> Enum.with_index(1)
    |> Enum.map(fn {bet, rank} -> bet * rank end)
    |> Enum.sum()
  end

  def p2(_input) do
  end

  def read_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      line
      |> words()
      |> map_list([&to_numeric_hand/1, &String.to_integer/1])
    end)
  end

  def to_numeric_hand(str) do
    str
    |> letters()
    |> Enum.map(fn
      "A" -> 14
      "K" -> 13
      "Q" -> 12
      "J" -> 11
      "T" -> 10
      n -> String.to_integer(n)
    end)
  end

  def compare_hands_1([counts, hand_a, _], [counts, hand_b, _]), do: hand_a <= hand_b
  def compare_hands_1([counts_a, _, _], [counts_b, _, _]), do: counts_a <= counts_b
end
