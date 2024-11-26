import AOC

aoc 2015, 19 do
  def input() do
    [replacements_string, starting_molecule] = String.split(input_string(), "\n\n", trim: true)

    starting_molecule = String.trim(starting_molecule)

    replacement_mappings =
      replacements_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        [from, to] = String.split(line, " => ", trim: true)
        {from, to}
      end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))

    {replacement_mappings, starting_molecule}
  end

  def possible_substitutions("", _, _), do: []

  def possible_substitutions(str, source, replacement) do
    <<curr::binary-size(1), next::binary>> = str

    this =
      if String.starts_with?(str, source),
        do: [String.replace(str, source, replacement, global: false)],
        else: []

    this ++ for r <- possible_substitutions(next, source, replacement), do: curr <> r
  end

  def possible_replacements(mappings, molecule) do
    for {source, replacements} <- mappings, replacement <- replacements do
      {source, replacement}
    end
    |> Enum.flat_map(fn {source, replacement} ->
      possible_substitutions(molecule, source, replacement)
    end)
    |> Enum.uniq()
  end

  def p1 do
    {mappings, molecule} = input()

    possible_replacements(mappings, molecule)
    |> length()
  end

  def p2 do
    {mappings, molecule} = input()

    find_shortest_path(molecule, invert(mappings))
  end

  defp find_shortest_path(molecule, replacements, steps \\ 0)
  defp find_shortest_path("e", _replacements, steps), do: steps

  defp find_shortest_path(molecule, replacements, steps) do
    {from, to} =
      Enum.find(replacements, fn {from, _} ->
        String.contains?(molecule, from)
      end)

    molecule = String.replace(molecule, from, to, global: false)

    find_shortest_path(molecule, replacements, steps + 1)
  end

  defp invert(mappings) do
    for {from, tos} <- mappings, to <- tos do
      {to, from}
    end
    |> Enum.sort_by(fn {from, _} -> String.length(from) end, :desc)
  end
end
