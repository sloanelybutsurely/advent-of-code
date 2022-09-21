import AOC

aoc 2021, 20 do
  import Integer, only: [is_even: 1]

  def p1 do
    {algorithm, source_image} = input()

    enhance(source_image, algorithm, 2)
    |> Map.values()
    |> Enum.count(&(&1))
  end

  def p2 do
    {algorithm, source_image} = input()

    enhance(source_image, algorithm, 50)
    |> Map.values()
    |> Enum.count(&(&1))
  end

  def enhance(image, algorithm, to_iteration, iteration \\ 0)
  def enhance(image, _algorithm, iteration, iteration), do: image

  def enhance(image, algorithm, to_iteration, iteration) do
    infinite_expanse = if is_even(iteration), do: Map.get(algorithm, 511), else: Map.get(algorithm, 0)

    {i_range, j_range} = bounds(image)

    for i <- i_range, j <- j_range, into: %{} do
      {{i, j}, Map.get(algorithm, algorithm_idx(image, {i, j}, infinite_expanse))}
    end
    |> enhance(algorithm, to_iteration, iteration + 1)
  end

  def bounds(image) do
    {{i_min, j_min}, {i_max, j_max}} =
      image
      |> Map.keys()
      |> Enum.min_max()

    {Range.new(i_min - 2, i_max + 2), Range.new(j_min - 2, j_max + 2)}
  end

  def algorithm_idx(image, {i, j}, infinite_expanse) do
    [
      {i - 1, j - 1},
      {i - 1, j},
      {i - 1, j + 1},
      {i, j - 1},
      {i, j},
      {i, j + 1},
      {i + 1, j - 1},
      {i + 1, j},
      {i + 1, j + 1}
    ]
    |> Enum.map(&Map.get(image, &1, infinite_expanse))
    |> Enum.map(fn
      true -> 1
      false -> 0
    end)
    |> Integer.undigits(2)
  end


  def input() do
    [unparsed_algorithm, unparsed_source_image] =
      input_string()
      |> String.split("\n\n", trim: true)

    algorithm =
      unparsed_algorithm
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn
        {"#", i} -> {i, true}
        {".", i} -> {i, false}
      end)
      |> Enum.into(%{})

    source_image =
      unparsed_source_image
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.split("", trim: true)
        |> Enum.map(fn
          "#" -> true
          "." -> false
        end)
        |> Enum.with_index()
      end)
      |> Enum.with_index()
      |> then(&for(
        {row, i} <- &1,
        {cell, j} <- row,
        into: %{},
        do: {{i, j}, cell}
      ))


    {algorithm, source_image}
  end
end
