import AOC

aoc 2024, 5 do
  def p1(input) do
    {rules, updates} = parse_input(input)

    updates
    |> Enum.filter(&sorted?(&1, rules))
    |> Enum.map(&median/1)
    |> Enum.sum()
  end

  def p2(input) do
    {rules, updates} = parse_input(input)

    updates
    |> Enum.reject(&sorted?(&1, rules))
    |> Enum.map(fn update ->
      Enum.sort(update, &sort_update(&1, &2, rules))
    end)
    |> Enum.map(&median/1)
    |> Enum.sum()
  end

  def sorted?(update, rules) do
    update == Enum.sort(update, &sort_update(&1, &2, rules))
  end

  def median(update) do
    Enum.at(update, div(length(update), 2))
  end

  def sort_update(a, b, rules) do
    a_before = Map.get(rules, a, [])
    b in a_before
  end

  ## input parsing

  def parse_input(input) do
    [rules_str, updates_str] = String.split(input, "\n\n")

    rules = parse_rules(rules_str)
    updates = parse_updates(updates_str)

    {rules, updates}
  end

  def parse_rules(rules_str) do
    rules_str
    |> String.split("\n", trim: true)
    |> Enum.group_by(
      &String.to_integer(binary_part(&1, 0, 2)),
      &String.to_integer(binary_part(&1, 3, 2))
    )
  end

  def parse_updates(updates_str) do
    updates_str
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)
    end)
  end
end
