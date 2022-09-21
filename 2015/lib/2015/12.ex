import AOC

aoc 2015, 12 do
  def p1 do
    input_stream()
    |> Stream.flat_map(fn l ->
      Regex.scan(~r/-?\d+/, l)
      |> List.flatten()
    end)
    |> Stream.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def p2 do
    input_string()
    |> String.trim()
    |> Jason.decode!()
    |> count_document()
  end

  def count_document(document) when is_map(document) do
    values = Map.values(document)

    if Enum.member?(values, "red") do
      0
    else
      document
      |> Map.values()
      |> Enum.map(&count_document/1)
      |> Enum.sum()
    end
  end

  def count_document(document) when is_list(document) do
    document
    |> Enum.map(&count_document/1)
    |> Enum.sum()
  end

  def count_document(document) when is_number(document), do: document
  def count_document(_), do: 0
end
