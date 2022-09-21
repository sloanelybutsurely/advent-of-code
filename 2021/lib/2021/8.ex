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

  def infer_mappings(words) do
    %{
      2 => [one],
      3 => [seven],
      4 => [four],
      5 => two_three_five,
      6 => zero_six_nine,
      7 => [eight]
    } = Enum.group_by(words, &length/1)

    {[three], two_five} = two_three_five |> Enum.split_with(&Enum.all?(one, fn x -> x in &1 end))

    [b] = four -- three

    {[five], [two]} = two_five |> Enum.split_with(&(b in &1))

    {[nine], six_zero} = zero_six_nine |> Enum.split_with(&Enum.all?(four, fn x -> x in &1 end))
    {[zero], [six]} = six_zero |> Enum.split_with(&Enum.all?(one, fn x -> x in &1 end))

    %{
      Enum.sort(zero) => 0,
      Enum.sort(one) => 1,
      Enum.sort(two) => 2,
      Enum.sort(three) => 3,
      Enum.sort(four) => 4,
      Enum.sort(five) => 5,
      Enum.sort(six) => 6,
      Enum.sort(seven) => 7,
      Enum.sort(eight) => 8,
      Enum.sort(nine) => 9
    }
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

      for word <- output do
        Map.get(mappings, Enum.sort(word))
      end
      |> Integer.undigits()
    end)
    |> Enum.sum()
  end
end
