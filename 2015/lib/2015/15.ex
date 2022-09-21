import AOC

aoc 2015, 15 do
  def parse_ingredient(str) do
    [name | properties] =
      Regex.run(
        ~r/(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$/,
        str,
        capture: :all_but_first
      )

    [capacity, durability, flavor, texture, calories] =
      properties |> Enum.map(&String.to_integer/1)

    {name, [capacity, durability, flavor, texture], calories}
  end

  def p1 do
    input_stream()
    |> Stream.map(&parse_ingredient/1)
    |> Enum.to_list()
  end

  def p2 do
  end
end
