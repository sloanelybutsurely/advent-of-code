import AOC

aoc 2024, 3 do
  import NimbleParsec

  def p1(input) do
    input
    |> read_progam()
    |> Enum.map(&eval/1)
    |> Enum.sum()
  end

  def p2(input) do
    {answer, _} =
      input
      |> read_progam()
      |> Enum.reduce({0, :do}, fn
        {:dont, _}, {sum, _} -> {sum, :dont}
        {:do, _}, {sum, _} -> {sum, :do}
        _inst, {_sum, :dont} = acc -> acc
        inst, {sum, :do} -> {sum + eval(inst), :do}
      end)

    answer
  end

  defcombinatorp(
    :mul,
    "mul("
    |> string()
    |> ignore()
    |> integer(min: 1)
    |> ignore(string(","))
    |> integer(min: 1)
    |> ignore(string(")"))
    |> tag(:mul)
  )

  defcombinatorp(
    :do,
    "do()"
    |> string()
    |> ignore()
    |> tag(:do)
  )

  defcombinatorp(
    :dont,
    "don't()"
    |> string()
    |> ignore()
    |> tag(:dont)
  )

  defparsecp(
    :program_parsec,
    [
      parsec(:mul),
      parsec(:do),
      parsec(:dont)
    ]
    |> choice()
    |> eventually()
    |> repeat()
  )

  def read_progam(input) do
    {:ok, program, _rest, _, _, _} = program_parsec(input)

    program
  end

  defp eval({:mul, [a, b]}), do: a * b
  defp eval({:do, _}), do: 0
  defp eval({:dont, _}), do: 0
end
