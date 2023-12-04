import AOC

aoc 2023, 4 do
  def p1(input) do
    input
    |> String.split("\n")
    |> Stream.map(&parse_line/1)
    |> Stream.map(&score_1/1)
    |> Enum.sum()
  end

  def p2(input) do
    cards =
      input
      |> String.split("\n")
      |> Enum.map(&parse_line/1)
      |> Enum.map(&score_2/1)

    card_counts =
      cards
      |> Stream.map(fn {id, _} -> {id, 1} end)
      |> Enum.into(%{})

    cards
    |> Enum.reduce(card_counts, fn {id, winners}, counts ->
      if winners > 0 do
        copies = Map.get(counts, id)

        copied_range =
          (id + 1)..(id + winners)

        for copy_id <- copied_range, reduce: counts do
          counts ->
            if Map.has_key?(counts, copy_id) do
              Map.update!(counts, copy_id, &(&1 + copies))
            else
              counts
            end
        end
      else
        counts
      end
    end)
    |> Map.values()
    |> Enum.sum()
  end

  def parse_line(line) do
    ["Card " <> card_string, numbers_string] = String.split(line, ": ")
    [winning_string, you_have_string] = String.split(numbers_string, " | ")

    card =
      card_string
      |> String.trim()
      |> String.to_integer()

    winning =
      winning_string
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer/1)

    you_have =
      you_have_string
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer/1)

    {card, winning, you_have}
  end

  def score_1({_, winning, you_have}) do
    number_of_winners =
      winning
      |> Enum.filter(&Enum.member?(you_have, &1))
      |> Enum.count()

    if number_of_winners < 1 do
      0
    else
      2 ** (number_of_winners - 1)
    end
  end

  def score_2({id, winning, you_have}) do
    number_of_winners =
      winning
      |> Enum.filter(&Enum.member?(you_have, &1))
      |> Enum.count()

    {id, number_of_winners}
  end
end
