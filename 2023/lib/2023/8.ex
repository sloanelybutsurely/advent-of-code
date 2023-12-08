import AOC
import AOCHelpers

aoc 2023, 8 do
  def p1(input) do
    {instructions, mappings} = read_input(input)

    all_but_last_step =
      instructions
      |> Stream.cycle()
      |> Stream.scan("AAA", &step(&1, &2, mappings))
      |> Stream.take_while(&(&1 != "ZZZ"))
      |> Enum.count()

    all_but_last_step + 1
  end

  def p2(input) do
    {instructions, mappings} = read_input(input)

    starts =
      mappings
      |> Map.keys()
      |> Enum.filter(&String.ends_with?(&1, "A"))

    lengths =
      starts
      |> Enum.map(fn start ->
        n =
          instructions
          |> Stream.cycle()
          |> Stream.scan(start, &step(&1, &2, mappings))
          |> Stream.take_while(&(not String.ends_with?(&1, "Z")))
          |> Enum.count()

        n + 1
      end)

    Enum.reduce(lengths, fn a, b ->
      div(a * b, Integer.gcd(a, b))
    end)
  end

  def step("L", curr, mappings) do
    mappings
    |> Map.get(curr)
    |> elem(0)
  end

  def step("R", curr, mappings) do
    mappings
    |> Map.get(curr)
    |> elem(1)
  end

  def step_all(inst, currs, mappings) do
    Enum.map(currs, &step(inst, &1, mappings))
  end

  def read_input(input) do
    [instructions_string, _ | mapping_lines] = lines(input)
    instructions = letters(instructions_string)

    mappings =
      mapping_lines
      |> Enum.map(fn mapping_line ->
        [_, key, left, right] = Regex.run(~r/^(.+) = \((.+), (.+)\)$/, mapping_line)

        {key, {left, right}}
      end)
      |> Enum.into(%{})

    {instructions, mappings}
  end
end
