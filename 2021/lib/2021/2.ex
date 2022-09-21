import AOC

aoc 2021, 2 do
  def input_stream(),
    do:
      super()
      |> Stream.map(fn
        "forward " <> v -> {:forward, String.to_integer(v)}
        "down " <> v -> {:down, String.to_integer(v)}
        "up " <> v -> {:up, String.to_integer(v)}
      end)

  def p1 do
    input_stream()
    |> Enum.reduce({0, 0}, fn
      {:forward, v}, {h, d} -> {h + v, d}
      {:down, v}, {h, d} -> {h, d + v}
      {:up, v}, {h, d} -> {h, d - v}
    end)
    |> then(fn {h, d} -> h * d end)
  end

  def p2 do
    input_stream()
    |> Enum.reduce({0, 0, 0}, fn
      {:forward, v}, {h, d, a} -> {h + v, d + a * v, a}
      {:down, v}, {h, d, a} -> {h, d, a + v}
      {:up, v}, {h, d, a} -> {h, d, a - v}
    end)
    |> then(fn {h, d, _v} -> h * d end)
  end
end
