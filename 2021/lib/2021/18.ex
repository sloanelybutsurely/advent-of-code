import AOC


aoc 2021, 18 do
  def p1 do
    input()
    |> Enum.reduce(fn x, acc -> concat(acc, x) |> reduce() end)
    |> to_lists()
    |> magnitude()
  end

  def p2 do
  end

  def magnitude([a, b]) do
    3 * magnitude(a) + 2 * magnitude(b)
  end
  def magnitude(n), do: n

  def reduce(encoded_snailfish_number) do
    case step(encoded_snailfish_number) do
      {:nop, result} -> result
      {_, result} -> reduce(result)
    end
  end

  def step([{dl, _} = left, {d, a}, {d, b}, right | rest]) when d > 4 and dl != d do
    {:exploded, [add(left, a), {d - 1, 0}, add(right, b) | rest]}
  end
  def step([{dl, _} = left, {d, a}, {d, _}]) when d > 4 and dl != d do
    {:exploded, [add(left, a), {d - 1, 0}]}
  end
  def step([{d, _}, {d, b}, right | rest]) when d > 4 do
    {:exploded, [{d - 1, 0}, add(right, b) | rest]}
  end

  def step([{d, n} | rest]) when n >= 10 do
    l = {d + 1, div(n, 2)}
    r = {d + 1, div(n, 2) + rem(n, 2)}
    {:split, [l, r | rest]}
  end

  def step([]) do
    {:nop, []}
  end
  def step([h | rest]) do
    {op, rest} = step(rest)
    {op, [h | rest]}
  end

  def add({d, n}, x), do: {d, n + x}

  def concat(xs, ys), do: deepen(xs ++ ys)

  def deepen(xs) do
    for {d, x} <- xs, do: {d + 1, x}
  end

  def to_lists(flat) do
    case to_lists_(flat) do
      {true, unflatter} -> to_lists(unflatter)
      {false, [{0, unflattest}]} -> unflattest
    end
  end

  def to_lists_([]), do: {false, []}
  def to_lists_([{d, a}, {d, b} | rest]) do
    {true, [{d - 1, [a, b]} | rest]}
  end
  def to_lists_([x | rest]) do
    {recurse, rest} = to_lists_(rest)
    {recurse, [x | rest]}
  end

  def encoded_to_string(encoded) do
    encoded
    |> to_lists()
    |> inspect(charlists: :as_lists)
    |> String.replace(" ", "")
  end

  def fst([a, _]), do: a
  def snd([_, b]), do: b


  def read_input_line(line, depth \\ 0)
  def read_input_line("[" <> rest, depth), do: read_input_line(rest, depth + 1)
  def read_input_line("]" <> rest, depth), do: read_input_line(rest, depth - 1)
  def read_input_line("," <> rest, depth), do: read_input_line(rest, depth)
  def read_input_line("", _), do: []
  def read_input_line(line, depth) do
    {n, rest} = Integer.parse(line)
    [{depth, n} | read_input_line(rest, depth)]
  end

  def input() do
    input_stream()
    |> Enum.map(&read_input_line/1)
  end

end
