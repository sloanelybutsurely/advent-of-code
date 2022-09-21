import AOC

aoc 2021, 18 do
  import Integer

  def p1 do
    input()
    |> Enum.reduce(fn x, acc -> reduce([acc, x]) end)
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

    IO.inspect(op, label: "op")
    IO.inspect(result, label: "result")

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
      {_, xs} -> xs
      xs -> xs
    end
  end
  def apply_op({:split, path}, xs), do: split(path, xs)

  def explode([:l], [a, b]) do
    [l, r] = a
    {{:l, l}, [0, add_car(b, r)]}
  end
  def explode([:r], [a, b]) do
    [l, r] = b
    {{:r, r}, [add_car(a, l), 0]}
  end
  def explode([:l | path], [a, b]) do
    case explode(path, a) do
      {{:r, r}, a} -> add_cdr([a, b], r)
      {op, a} -> {op, [a, b]}
      a -> [a, b]
    end
  end
  def explode([:r | path], [a, b]) do
    case explode(path, b) do
      {{:l, l}, b} -> add_car([a, b], l)
      {op, b} -> {op, [a, b]}
      b -> [a, b]
    end
  end

  def add_car([a, b], v), do: [add_cdr(a, v), b]
  def add_car(car, v), do: car + v
  def add_cdr([a, b], v), do: [a, add_car(b, v)]
  def add_cdr(cdr, v), do: cdr + v

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
