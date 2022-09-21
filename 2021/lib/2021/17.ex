import AOC

aoc 2021, 17 do
  def p1 do
    {target, {xb, yb} = bounds} = input_target()

    for xv <- xb..0, yv <- yb..-yb do
      launch_probe(xv, yv, target, bounds)
    end
    |> Enum.filter(&match?({true, _}, &1))
    |> Enum.map(&elem(&1, 1))
    |> Enum.map(&max_height/1)
    |> Enum.max()
  end

  def p2 do
    {target, {xb, yb} = bounds} = input_target()

    for xv <- xb..0, yv <- yb..-yb do
      launch_probe(xv, yv, target, bounds)
    end
    |> Enum.count(&match?({true, _}, &1))
  end

  def max_height(path) do
    path
    |> Enum.map(&elem(&1, 1))
    |> Enum.max()
  end

  def launch_probe(xv0, yv0, target, bounds) do
    path =
      probe_path(xv0, yv0)
      |> Enum.take_while(fn curr -> not past_target?(curr, bounds) end)

    {Enum.any?(path, &MapSet.member?(target, &1)), path}
  end

  def probe_path(xv0, yv0) do
    Stream.unfold({{0, 0}, {xv0, yv0}}, iterate(&next/1))
    |> Stream.map(&elem(&1, 0))
  end

  def iterate(fun), do: fn curr -> {curr, fun.(curr)} end

  def next({_, vel} = pos_vel), do: {next_pos(pos_vel), next_vel(vel)}

  def next_pos({{x, y}, {xv, yv}}), do: {x + xv, y + yv}

  def next_vel({xv, yv}), do: {next_xv(xv), next_yv(yv)}

  def next_xv(0), do: 0
  def next_xv(xv) when xv > 0, do: xv - 1
  def next_xv(xv) when xv < 0, do: xv + 1

  def next_yv(yv), do: yv - 1

  def past_target?({x, y}, {xb, yb}), do: past_target?(x, xb) or past_target?(y, yb)
  def past_target?(n, bound) when bound < 0, do: n < bound
  def past_target?(n, bound), do: n > bound

  def input_target(input_string \\ input_string()) do
    [xs, ys] =
      ~r/target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/
      |> Regex.run(input_string, capture: :all_but_first)
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(2)
      |> Enum.map(fn args -> apply(&Range.new/2, args) end)

    target = for x <- xs, y <- ys, into: MapSet.new(), do: {x, y}
    bounds = Enum.max_by(target, fn {x, y} -> abs(x) + abs(y) end)
    {target, bounds}
  end
end
