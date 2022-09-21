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

  def infer_mappings(words) do
    one = Enum.find(words, &(length(&1) == 2)) |> MapSet.new()
    seven = Enum.find(words, &(length(&1) == 3)) |> MapSet.new()
    eight = Enum.find(words, &(length(&1) == 7)) |> MapSet.new()
    four = Enum.find(words, &(length(&1) == 4)) |> MapSet.new()

    bd = MapSet.difference(four, one)

    [six | two_five] =
      Enum.reject(words, fn word ->
        MapSet.subset?(one, MapSet.new(word))
      end)
      |> Enum.sort_by(&length/1, :desc)
      |> Enum.map(&MapSet.new/1)

    [c] = MapSet.difference(eight, six) |> MapSet.to_list()
    [f] = MapSet.delete(one, c) |> MapSet.to_list()

    nine =
      Enum.find(words, fn word ->
        length(word) == 6 and not MapSet.equal?(MapSet.new(word), six)
      end)
      |> MapSet.new()

    [e] = MapSet.difference(eight, nine) |> MapSet.to_list()

    five = MapSet.delete(six, e)
    two = Enum.find(two_five, &(not MapSet.equal?(&1, five)))

    three = two |> MapSet.delete(e) |> MapSet.put(f)

    [b] = MapSet.difference(nine, three) |> MapSet.to_list()
    [d] = MapSet.delete(bd, b) |> MapSet.to_list()

    zero = MapSet.delete(eight, d)

    %{
      MapSet.to_list(zero) => 0,
      MapSet.to_list(one) => 1,
      MapSet.to_list(two) => 2,
      MapSet.to_list(three) => 3,
      MapSet.to_list(four) => 4,
      MapSet.to_list(five) => 5,
      MapSet.to_list(six) => 6,
      MapSet.to_list(seven) => 7,
      MapSet.to_list(eight) => 8,
      MapSet.to_list(nine) => 9
    }
  end

  def rewire(mappings, word), do: Enum.map(word, &Map.get(mappings, &1, ?x)) |> Enum.sort()

  def render_mappings(mappings) do
    s = 32
    IO.inspect(mappings)
    [a, b, c, d, e, f, g] = ?a..?g |> Enum.map(&Map.get(mappings, &1, ?x))

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
      mappings = infer_mappings(signals)

      output
      |> Enum.map(&Map.get(mappings, &1))
      |> Integer.undigits()
      |> IO.inspect()
    end)
    |> Enum.sum()
  end
end
