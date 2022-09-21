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

  def splits(xs) do
    for i <- 1..(length(xs) - 1) do
      Enum.split(xs, i)
    end
  end

  def possible_replacements(mappings, str) do
    for {source, replacements} <- mappings,
        replacement <- replacements,
        {l, r} <- splits(String.split(str, source)),
        uniq: true do
      Enum.join(
        [
          Enum.join(l, source),
          Enum.join(r, source)
        ],
        replacement
      )
    end
  end

  def p1 do
    {mappings, molecule} = input()

    possible_replacements(mappings, molecule)
    |> length()
  end

  def p2 do
  end
end
