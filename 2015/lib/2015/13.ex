import AOC

aoc 2015, 13 do
  import NimbleParsec

  def p1 do
    {guests, edges} = parse_input()

    permutations(guests)
    |> Enum.map(&happiness(edges, &1))
    |> Enum.max()
  end

  def p2 do
    {guests, edges} = parse_input()

    guests = guests ++ ["Zach"]

    permutations(guests)
    |> Enum.map(&happiness(edges, &1))
    |> Enum.max()
  end

  defparsec(
    :fact_parsec,
    ascii_string([?a..?z, ?A..?Z], min: 1)
    |> ignore(string(" would "))
    |> choice([
      string("gain"),
      string("lose")
    ])
    |> ignore(string(" "))
    |> integer(min: 1)
    |> ignore(string(" happiness units by sitting next to "))
    |> ascii_string([?a..?z, ?A..?Z], min: 1)
    |> ignore(string("."))
  )

  def parse_fact(str) do
    {:ok, [left, sign, amount, right], _, _, _, _} = fact_parsec(str)

    {{left, right}, edge_value(sign, amount)}
  end

  def edge_value("gain", v), do: v
  def edge_value("lose", v), do: 0 - v

  def sort({a, b} = p) when a > b, do: swap(p)
  def sort(p), do: p
  def swap({a, b}), do: {b, a}

  def parse_input() do
    edges =
      input_stream()
      |> Stream.map(&parse_fact/1)
      |> Enum.into(%{})

    guests =
      edges
      |> Map.keys()
      |> Enum.reduce(MapSet.new(), fn {a, b}, acc -> acc |> MapSet.put(a) |> MapSet.put(b) end)

    edges =
      for {pair, weight} <- edges, reduce: %{} do
        acc ->
          sorted = sort(pair)

          if Map.has_key?(acc, sorted) do
            acc
          else
            reverse = swap(pair)
            Map.put(acc, sorted, weight + Map.get(edges, reverse))
          end
      end

    guests = Enum.to_list(guests)

    {guests, edges}
  end

  def permutations([]), do: [[]]

  def permutations(xs) do
    for h <- xs, t <- permutations(xs -- [h]), do: [h | t]
  end

  def happiness(edges, [a | _] = xs) do
    happiness(edges, xs, a, 0)
  end

  def happiness(edges, [t], h, acc) do
    edge = Map.get(edges, sort({h, t}), 0)
    edge + acc
  end

  def happiness(edges, [a, b | rest], h, acc) do
    edge = Map.get(edges, sort({a, b}), 0)
    happiness(edges, [b | rest], h, acc + edge)
  end
end
