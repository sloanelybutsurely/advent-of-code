import AOC

aoc 2024, 13 do
  def p1(input) do
    input
    |> read_machines()
    |> Enum.map(&solve/1)
    |> Enum.filter(&match?({:solution, a, b} when a <= 100 and b <= 100, &1))
    |> Enum.map(&tokens_for_solution/1)
    |> Enum.sum()
  end

  def p2(input) do
    input
    |> read_machines()
    |> Enum.map(fn %{goal: {x, y}} = machine ->
      %{machine | goal: {x + 10_000_000_000_000, y + 10_000_000_000_000}}
    end)
    |> Enum.map(&solve/1)
    |> Enum.map(&tokens_for_solution/1)
    |> Enum.sum()
  end

  defp solve(%{a: {ax, ay}, b: {bx, by}, goal: {x, y}}) do
    # a * ax + b * bx = x
    # a * ay + b * by = y

    # a * ax + b * bx = x
    #          a * ax = x - b * bx
    #               a = (x - b * bx) / ax

    #                        a * ay + b * by = y
    #      ((x - b * bx) / ax) * ay + b * by = y
    #    ((ay * (x - b * bx)) / ax) + b * by = y
    # ((ay * x - ay * b * bx) / ax) + b * by = y
    #     ay * x - ay * b * bx + b * by * ax = y * ax
    #              b * by * ax - b * ay * bx = y * ax - x * ay
    #                b * (by * ax - ay * bx) = y * ax - x * ay
    #                                      b = (y * ax - x * ay) / (by * ax - ay * bx)

    b = div(y * ax - x * ay, by * ax - ay * bx)
    a = div(x - b * bx, ax)

    if a * ax + b * bx == x and a * ay + b * by == y do
      {:solution, a, b}
    else
      :impossible
    end
  end

  defp tokens_for_solution({:solution, a, b}) do
    3 * a + b
  end

  defp tokens_for_solution(_), do: 0

  ## input

  defp read_machines(str) do
    ~r/Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)/m
    |> Regex.scan(
      str,
      capture: :all_but_first
    )
    |> Enum.map(fn strings ->
      [xa, ya, xb, yb, x, y] =
        Enum.map(strings, &String.to_integer/1)

      %{
        a: {xa, ya},
        b: {xb, yb},
        goal: {x, y}
      }
    end)
  end
end
