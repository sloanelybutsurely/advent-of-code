import AOC

aoc 2023, 5 do
  def p1(input) do
    {seeds, maps} = read_input(input)

    seeds
    |> Enum.map(&traverse_maps(maps, &1))
    |> Enum.min()
  end

  def p2(_input) do
  end

  def traverse_maps(maps, start) do
    Enum.reduce(maps, start, &traverse_map/2)
  end

  def traverse_map(ranges_and_deltas, start) do
    case Enum.find(ranges_and_deltas, fn {range, _} -> start in range end) do
      {_, delta} -> start - delta
      nil -> start
    end
  end

  def read_input(input) do
    ["seeds: " <> seeds_str, _ | map_strs] = String.split(input, "\n")

    map_chunks =
      map_strs
      |> chunk_by_removing("")
      |> Enum.map(&Enum.drop(&1, 1))
      |> map_map(&to_integer_list/1)
      |> map_map(&to_range_and_delta/1)

    {to_integer_list(seeds_str), map_chunks}
  end

  def chunk_by_removing(enum, pattern) do
    Enum.chunk_while(
      enum,
      [],
      fn
        ^pattern, acc -> {:cont, Enum.reverse(acc), []}
        elem, acc -> {:cont, [elem | acc]}
      end,
      &{:cont, Enum.reverse(&1), []}
    )
  end

  def to_integer_list(str) do
    str
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  def to_range_and_delta([dest_start, source_start, len]) do
    source_range = source_start..(source_start + len)
    delta = source_start - dest_start

    {source_range, delta}
  end

  def map_map(enum_of_enums, fun), do: Enum.map(enum_of_enums, fn enum -> Enum.map(enum, fun) end)
end
