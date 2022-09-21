import AOC

aoc 2021, 18 do
  import Integer

  def p1 do
    input()
    |> Enum.reduce(fn x, acc ->
      result = reduce([acc, x])
      IO.write(".")
      result
    end)
    |> magnitude()
  end

  def p2 do
  end

  def magnitude([a, b]) do
    3 * magnitude(a) + 2 * magnitude(b)
  end
  def magnitude(x), do: x

  def reduce(xs) do
    op = find_op(xs)

    recurse = op != :none
    result = apply_op(op, xs)

    # IO.inspect(op, label: "op")
    # IO.inspect(result, label: "result")

    if recurse, do: reduce(result), else: result
  end

  def find_op(xs, depth \\ 0, path \\ [])
  def find_op(a, _depth, path) when is_number(a) and a >= 10, do: {:split, path}
  def find_op(a, _depth, _path) when is_number(a), do: :none
  def find_op([a, b], depth, path) when is_number(a) and is_number(b) and depth >= 4, do: {:explode, path}
  def find_op([a, b], depth, path) do
    case {find_op(a, depth + 1, path ++ [:l]), find_op(b, depth + 1, path ++ [:r])} do
      {:none, op} -> op
      {op, _} -> op
    end
  end

  def apply_op(:none, xs), do: xs
  def apply_op({:explode, path}, xs) do
    case explode(path, xs) do
      # {_, xs} -> xs
      xs -> xs
    end
  end
  def apply_op({:split, path}, xs), do: split(path, xs)

  def explode(path, xs) do
    {adds, xs} = zero_out_exploded(path, xs)
    add_exploded(path, adds, xs)
  end

  def zero_out_exploded([], [a, b]), do: {[a, b], 0}
  def zero_out_exploded([:l | path], [a, b]) do
    {ret, a} = zero_out_exploded(path, a)
    {ret, [a, b]}
  end
  def zero_out_exploded([:r | path], [a, b]) do
    {ret, b} = zero_out_exploded(path, b)
    {ret, [a, b]}
  end

  def add_exploded([:r], [l, _], [a, b]) do
    [add_r(a, l), b]
  end
  def add_exploded([:l], [_, r], [a, b]) do
    [a, add_l(b, r)]
  end
  def add_exploded([:l | path], [_, r] = adds, [a, b]) do
    if :l in path do
      [add_exploded(path, adds, a), b]
    else
      [add_exploded(path, adds, a), add_l(b, r)]
    end
  end
  def add_exploded([:r | path], [l, _] = adds, [a, b]) do
    if :r in path do
      [a, add_exploded(path, adds, b)]
    else
      [add_r(a, l), add_exploded(path, adds, a)]
    end
  end
  def add_exploded(_, _, other), do: other

  def add_r([a, b], n), do: [a, add_r(b, n)]
  def add_r(b, n), do: b + n
  def add_l([a, b], n), do: [add_l(a, n), b]
  def add_l(a, n), do: a + n

  def split([], v) when is_even(v), do: [div(v, 2), div(v, 2)]
  def split([], v) when is_odd(v), do: [div(v, 2), div(v, 2) + 1]
  def split([:l | path], [a, b]), do: [split(path, a), b]
  def split([:r | path], [a, b]), do: [a, split(path, b)]

  def input() do
    input_stream()
    |> Stream.map(&Code.eval_string/1)
    |> Stream.map(&elem(&1, 0))
  end
end
