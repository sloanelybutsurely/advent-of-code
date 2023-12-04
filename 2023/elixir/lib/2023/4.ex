import AOC

aoc 2023, 4 do
  def p1(input) do
    input
    |> String.split("\n")
    |> Stream.map(&parse_line/1)
    |> Stream.map(&score_1/1)
    |> Enum.sum()
  end

  def p2(_input) do
    # input
    # |> String.split("\n")
    # |> Enum.map(&parse_line/1)
    # |> Enum.map(&score_2/1)
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
