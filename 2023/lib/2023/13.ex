import AOC
import AOCHelpers

aoc 2023, 13 do
  def p1(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(&lines_of_chars/1)
    |> Enum.map(&summarize/1)
    |> Enum.sum()
  end

  def p2(input) do
    input
    |> String.split("\n\n")
    |> Enum.map(&lines_of_chars/1)
    |> Enum.map(&summarize(&1, 1))
    |> Enum.sum()
  end

  def summarize(lines, smudges \\ 0) do
    vertical_line_of_reflection = find_reflection_index(lines, smudges)
    horizontal_line_of_reflection = lines |> transpose() |> find_reflection_index(smudges)

    summary = vertical_line_of_reflection + 100 * horizontal_line_of_reflection

    summary
  end

  def find_reflection_index(lines, smudges) do
    len = lines |> List.first() |> length()

    idx =
      for i <- 1..(len - 1) do
        lines
        |> Enum.map(fn line ->
          line
          |> Enum.split(i)
          |> Tuple.to_list()
          |> map_list([&Enum.reverse/1, &id/1])
          |> Enum.zip_with(&apply(Kernel, :==, &1))
          |> Enum.count(is?(false))
        end)
        |> Enum.sum()
      end
      |> Enum.find_index(is?(smudges))

    if idx, do: idx + 1, else: 0
  end
end
