import AOC

aoc 2021, 6 do
  use AOCHelpers

  @doc """
  Emulates the fish literally as described by the problem but
  adds new (starting at 8) fish next to their parent instead
  of at the end of list.
  """
  def tick_literal([]), do: []
  def tick_literal([0 | xs]), do: [6, 8 | tick_literal(xs)]
  def tick_literal([x | xs]), do: [x - 1 | tick_literal(xs)]

  @doc """
  "Ticks" a day-frequency encoded list of fish counts to the next day.

  Formula for the next-day count:
    fish(8) = fish(0)
    fish(6) = fish(7) + fish(0)
    fish(n) = fish(n + 1)
  """
  def tick_encoded([f0, f1, f2, f3, f4, f5, f6, f7, f8]) do
    # 0,  1,  2,  3,  4,  5, 6,       7,  8
    [f1, f2, f3, f4, f5, f6, f7 + f0, f8, f0]
  end

  def p1 do
    start = input_number_list()

    iterate(80, start, &tick_literal/1)
    |> length()
  end

  def p2 do
    start = input_number_list()

    freqs =
      start
      |> Enum.frequencies()

    encoded_fish = for i <- 0..8, do: Map.get(freqs, i, 0)

    iterate(256, encoded_fish, &tick_encoded/1)
    |> Enum.sum()
  end
end
