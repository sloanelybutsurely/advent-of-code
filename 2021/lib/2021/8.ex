import AOC

aoc 2021, 8 do
  def parse_line(line) do
    [signal_patterns, output_value] = String.split(line, " | ")

    signal_patterns = signal_patterns |> String.split(" ") |> Enum.map(&String.to_charlist/1)
    output_value = output_value |> String.split(" ") |> Enum.map(&String.to_charlist/1)

    {signal_patterns, output_value}
  end

  def parse_input() do
    input_stream()
    |> Stream.map(&parse_line/1)
  end

  def decode_digit_count([_, _]), do: 1
  def decode_digit_count([_, _, _]), do: 1
  def decode_digit_count([_, _, _, _]), do: 1
  def decode_digit_count([_, _, _, _, _, _, _]), do: 1
  def decode_digit_count(_word), do: 0

  def decode_digit('abcefg'), do: 0
  def decode_digit('cf'), do: 1
  def decode_digit('acdeg'), do: 2
  def decode_digit('acdfg'), do: 3
  def decode_digit('bcdf'), do: 4
  def decode_digit('abdfg'), do: 5
  def decode_digit('abdefg'), do: 6
  def decode_digit('acf'), do: 7
  def decode_digit('abcdefg'), do: 8
  def decode_digit('abcdfg'), do: 9

  def add_mappings(chars, mappings \\ %{})
  def add_mappings(_, mappings) when map_size(mappings) == 7, do: mappings

  def add_mappings([c, f], mappings),
    do:
      mappings
      |> Map.put_new(c, ?c)
      |> Map.put_new(f, ?f)

  def add_mappings([a, c, f], mappings),
    do:
      mappings
      |> Map.put_new(a, ?a)
      |> Map.put_new(c, ?c)
      |> Map.put_new(f, ?f)

  def add_mappings([b, c, d, f], mappings),
    do:
      mappings
      |> Map.put_new(b, ?b)
      |> Map.put_new(c, ?c)
      |> Map.put_new(d, ?d)
      |> Map.put_new(f, ?f)

  def add_mappings([a, b, c, d, e, f, g], mappings),
    do:
      mappings
      |> Map.put_new(a, ?a)
      |> Map.put_new(b, ?b)
      |> Map.put_new(c, ?c)
      |> Map.put_new(d, ?d)
      |> Map.put_new(e, ?e)
      |> Map.put_new(f, ?f)
      |> Map.put_new(g, ?g)

  def add_mappings(_, mappings), do: mappings

  def infer_mappings(words),
    do:
      words
      |> Enum.sort_by(&length/1)
      |> IO.inspect()
      |> Enum.reduce(%{}, &add_mappings/2)

  def rewire(mappings, word), do: Enum.map(word, &Map.get(mappings, &1, ?x)) |> Enum.sort()

  def render_mappings(mappings) do
    s = 32
    IO.inspect(mappings)
    [a, b, c, d, e, f, g] = ?a..?g |> Enum.map(&Map.get(mappings, &1))

    Enum.map(
      [
        [s, a, a, a, a, s],
        [b, s, s, s, s, c],
        [b, s, s, s, s, c],
        [s, d, d, d, d, s],
        [e, s, s, s, s, f],
        [e, s, s, s, s, f],
        [s, g, g, g, g, s]
      ],
      &IO.chardata_to_string/1
    )
    |> Enum.join("\n")
    |> IO.puts()
  end

  def p1 do
    parse_input()
    |> Enum.map(fn {_, output} ->
      output
      |> Enum.map(&decode_digit_count/1)
      |> Enum.sum()
    end)
    |> Enum.sum()
  end

  def p2 do
    parse_input()
    |> Enum.map(fn {signals, output} ->
      mappings = infer_mappings(signals ++ output)

      output
      |> Enum.map(&rewire(mappings, &1))
      |> Enum.map(&decode_digit/1)
      |> Integer.undigits()
      |> IO.inspect()
    end)
    |> Enum.sum()
  end
end
