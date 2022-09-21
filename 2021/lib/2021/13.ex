import AOC

aoc 2021, 13 do
  def input(input_string \\ input_string()) do
    [dots_string, folds_string] =
      input_string
      |> String.split("\n\n", trim: true)

    dots =
      dots_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        [x, y] =
          String.split(line, ",")
          |> Enum.map(&String.to_integer/1)

        {x, y}
      end)
      |> MapSet.new()

    folds =
      folds_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn
        "fold along y=" <> y -> {:y, String.to_integer(y)}
        "fold along x=" <> x -> {:x, String.to_integer(x)}
      end)

    {dots, folds}
  end

  def do_fold({:y, line}, dots) do
    for {x, y} <- dots, into: MapSet.new() do
      if y > line do
        {x, line - (y - line)}
      else
        {x, y}
      end
    end
  end

  def do_fold({:x, line}, dots) do
    for {x, y} <- dots, into: MapSet.new() do
      if x > line do
        {line - (x - line), y}
      else
        {x, y}
      end
    end
  end

  def p1 do
    {dots, [fold | _folds]} = input()

    do_fold(fold, dots) |> MapSet.size()
  end

  def p2 do
    {dots, folds} = input()

    final_dots = Enum.reduce(folds, dots, &do_fold/2)

    max_x = final_dots |> Enum.map(&elem(&1, 0)) |> Enum.max()
    max_y = final_dots |> Enum.map(&elem(&1, 1)) |> Enum.max()

    for x <- 0..max_x, into: "" do
      for y <- 0..max_y, into: "" do
        if MapSet.member?(final_dots, {x, y}), do: "#", else: "."
      end <> "\n"
    end
    |> IO.puts()
  end
end
