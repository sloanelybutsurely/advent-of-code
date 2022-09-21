import AOC

aoc 2021, 7 do
  use AOCHelpers

  def p1 do
    crabs = input_number_list()

    max = Enum.max(crabs)

    for position <- 1..max do
      for crab <- crabs do
        abs(crab - position)
      end
      |> Enum.sum()
    end
    |> Enum.min()
  end

  def p2 do
    crabs = input_number_list()

    max = Enum.max(crabs)

    for position <- 1..max do
      for crab <- crabs do
        n = abs(crab - position)
        1 / 2 * n * (n + 1)
      end
      |> Enum.sum()
    end
    |> Enum.min()
    |> trunc()
  end
end
