import AOC
import AOC.Prelude

aoc 2024, 7 do
  def p1(input) do
    ops = [&Kernel.+/2, &Kernel.*/2]
    calibration_result(input, ops)
  end

  def p2(input) do
    ops = [&Kernel.+/2, &Kernel.*/2, &String.to_integer("#{&1}#{&2}")]
    calibration_result(input, ops)
  end

  defp calibration_result(input, ops) do
    input
    |> read_equations()
    |> Enum.filter(&solvable?(&1, ops))
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  defp solvable?({answer, [acc | nums]}, ops), do: solvable?(nums, ops, answer, acc)

  defp solvable?([], _ops, answer, answer), do: true
  defp solvable?([], _ops, _answer, _acc), do: false
  defp solvable?(_nums, _ops, answer, acc) when acc > answer, do: false

  defp solvable?([n | rest], ops, answer, acc) do
    Enum.any?(ops, &solvable?(rest, ops, answer, &1.(acc, n)))
  end

  ## input

  defp read_equations(input) do
    input
    |> lines()
    |> Enum.map(&String.split(&1, ": "))
    |> Enum.map(fn groups ->
      [&String.to_integer/1, &ints/1]
      |> Enum.zip_with(groups, &apply(&1, [&2]))
      |> List.to_tuple()
    end)
  end
end
