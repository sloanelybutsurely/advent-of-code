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
  end
end
