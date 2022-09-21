import AOC

aoc 2015, 1 do
  def input_chars(), do: input_string() |> String.split("", trim: true)

  def p1 do
    input_chars()
    |> Enum.reduce(0, fn
      "(", floor -> floor + 1
      ")", floor -> floor - 1
    end)
  end

  def p2 do
    input_string()
    |> find_step_entering_basement(0, 0)
  end

  defp find_step_entering_basement(")" <> _, 0, step), do: step + 1

  defp find_step_entering_basement("(" <> rest, floor, step),
    do: find_step_entering_basement(rest, floor + 1, step + 1)

  defp find_step_entering_basement(")" <> rest, floor, step),
    do: find_step_entering_basement(rest, floor - 1, step + 1)
end
