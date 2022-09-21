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

  def compile_ingredient({_label, coefficients, _cal}) do
    fn v -> coefficients |> Enum.map(&(&1 * v)) end
  end

  def p1 do
    ingredients =
      input_stream()
      |> Enum.map(&parse_ingredient/1)
      |> Enum.map(&compile_ingredient/1)

    for i <- 1..100,
        j <- 1..(100 - i),
        k <- 1..(100 - (i + j)),
        l <- 1..(100 - (i + j + k)),
        i + j + k + l == 100 do
      Enum.zip(ingredients, [i, j, k, l])
      |> Enum.map(fn {fun, arg} -> apply(fun, [arg]) end)
      |> List.zip()
      |> Enum.map(fn t -> max(0, Tuple.sum(t)) end)
      |> Enum.product()
    end
    |> Enum.sort(:desc)
    |> hd()
  end

  def p2 do
    ingredients =
      input_stream()
      |> Enum.map(&parse_ingredient/1)

    [m, n, o, p] = ingredients |> Enum.map(&elem(&1, 2))

    compiled_ingredients = Enum.map(ingredients, &compile_ingredient/1)

    for i <- 1..100,
        j <- 1..(100 - i),
        k <- 1..(100 - (i + j)),
        l <- 1..(100 - (i + j + k)),
        i + j + k + l == 100 and i * m + j * n + k * o + l * p == 500 do
      Enum.zip(compiled_ingredients, [i, j, k, l])
      |> Enum.map(fn {fun, arg} -> apply(fun, [arg]) end)
      |> List.zip()
      |> Enum.map(fn t -> max(0, Tuple.sum(t)) end)
      |> Enum.product()
    end
    |> Enum.sort(:desc)
    |> hd()
  end
end
