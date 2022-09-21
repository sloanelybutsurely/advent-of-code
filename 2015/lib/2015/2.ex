import AOC

aoc 2015, 2 do
  def dimensions_stream(),
    do:
      input_stream()
      |> Stream.map(fn line ->
        [l, w, h] = String.split(line, "x") |> Enum.map(&String.to_integer/1)
        {l, w, h}
      end)

  def p1 do
    dimensions_stream()
    |> Stream.map(fn {l, w, h} ->
      a = l * w
      b = w * h
      c = h * l
      2 * a + 2 * b + 2 * c + Enum.min([a, b, c])
    end)
    |> Enum.sum()
  end

  def p2 do
    dimensions_stream()
    |> Stream.map(fn {l, w, h} ->
      [x, y | _] = Enum.sort([l, w, h])

      2 * x + 2 * y + l * w * h
    end)
    |> Enum.sum()
  end
end
