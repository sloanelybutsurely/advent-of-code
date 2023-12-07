import AOC
import AOCHelpers

aoc 2023, 7 do
  def p1(input) do
    input
    |> read_input(fn
      "A" -> 14
      "K" -> 13
      "Q" -> 12
      "J" -> 11
      "T" -> 10
      n -> String.to_integer(n)
    end)
    |> Enum.map(fn [hand, bet] ->
      counts =
        hand
        |> Enum.frequencies()
        |> Map.values()
        |> Enum.sort(:desc)

      [counts, hand, bet]
    end)
    |> Enum.sort(&compare_hands/2)
    |> Enum.map(&Enum.at(&1, 2))
    |> Enum.with_index(1)
    |> Enum.map(fn {bet, rank} -> bet * rank end)
    |> Enum.sum()
  end

  def p2(input) do
    input
    |> read_input(fn
      "J" -> -1
      "A" -> 14
      "K" -> 13
      "Q" -> 12
      "T" -> 10
      n -> String.to_integer(n)
    end)
    |> Enum.map(fn [hand, bet] ->
      {jokers, hand_without_jokers} = Enum.split_with(hand, &(&1 === -1))

      number_of_jokers = Enum.count(jokers)

      counts_without_jokers =
        hand_without_jokers
        |> Enum.frequencies()
        |> Map.values()
        |> Enum.sort(:desc)

      counts = [
        List.first(counts_without_jokers, 0) + number_of_jokers
        | Enum.drop(counts_without_jokers, 1)
      ]

      [counts, hand, bet]
    end)
    |> Enum.sort(&compare_hands/2)
    |> Enum.map(&Enum.at(&1, 2))
    |> Enum.with_index(1)
    |> Enum.map(fn {bet, rank} -> bet * rank end)
    |> Enum.sum()
  end

  def read_input(input, card_to_number) do
    input
    |> lines()
    |> Enum.map(fn line ->
      line
      |> words()
      |> map_list([&to_numeric_hand(&1, card_to_number), &String.to_integer/1])
    end)
  end

  def to_numeric_hand(str, card_to_number) do
    str
    |> letters()
    |> Enum.map(card_to_number)
  end

  def compare_hands([counts_a, hand_a, _], [counts_b, hand_b, _]) do
    [counts_a, hand_a] <= [counts_b, hand_b]
  end
end
