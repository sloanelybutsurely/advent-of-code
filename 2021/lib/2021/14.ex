import AOC

aoc 2021, 14 do
  use AOCHelpers

  def input(input_string \\ input_string()) do
    [sequence, insertion_mappings_string] =
      input_string
      |> String.split("\n\n", trim: true)

    insertion_mappings =
      insertion_mappings_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        [from, to] = String.split(line, " -> ")
        {from, to}
      end)
      |> Map.new()

    {sequence, insertion_mappings}
  end

  def perform_insertions_lossy(sequence, mappings) when is_binary(sequence),
    do:
      sequence
      |> to_frequencies()
      |> perform_insertions_lossy(mappings)

  def perform_insertions_lossy({letter_freqs, pair_freqs}, mappings) do
    [pair_ops, letter_ops] =
      for {from, to} <- mappings,
          <<front::binary-size(1), back::binary-size(1)>> = from,
          Map.has_key?(pair_freqs, from) do
        existing_pairs = Map.get(pair_freqs, from, 0)

        {[{from, -existing_pairs}, {front <> to, existing_pairs}, {to <> back, existing_pairs}],
         [{to, existing_pairs}]}
      end
      |> Enum.unzip()
      |> Tuple.to_list()
      |> Enum.map(&List.flatten/1)

    letter_freqs = apply_ops(letter_freqs, letter_ops)
    pair_freqs = apply_ops(pair_freqs, pair_ops)

    {letter_freqs, pair_freqs}
  end

  def apply_ops(map, ops) do
    map
    |> Enum.concat(ops)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(fn {k, v} -> {k, Enum.sum(v)} end)
    |> Map.new()
  end

  def to_frequencies(sequence) do
    sequence_list = String.split(sequence, "", trim: true)
    pairs_list = Enum.zip_with(sequence_list, Enum.drop(sequence_list, 1), &<>/2)

    letter_frequencies = sequence_list |> Enum.frequencies()
    pair_frequencies = pairs_list |> Enum.frequencies()

    {letter_frequencies, pair_frequencies}
  end

  def calculate_score_of_iteration({starting_seq, mappings} \\ input(), n) do
    {final_letter_freqs, _} =
      iterate(n, starting_seq, fn sequence ->
        perform_insertions_lossy(sequence, mappings)
      end)

    {{_, min}, {_, max}} =
      final_letter_freqs
      |> Enum.min_max_by(&elem(&1, 1))

    max - min
  end

  def p1, do: calculate_score_of_iteration(10)
  def p2, do: calculate_score_of_iteration(40)
end
