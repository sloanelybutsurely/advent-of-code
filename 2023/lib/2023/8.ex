import AOC
import AOCHelpers

aoc 2023, 8 do
  def p1(input) do
    {instructions, mappings} = read_input(input)

    start = "AAA"
    finish = "ZZZ"

    all_but_last_step =
      instructions
      |> Stream.cycle()
      |> Stream.scan(start, &step(&1, &2, mappings))
      |> Stream.take_while(&(&1 != finish))
      |> Enum.count()

    all_but_last_step + 1
  end

  def p2(_input) do
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
