defmodule AdventOfCode.Day01 do
  @moduledoc """
  Day 1
  """

  def parse_input_stream(input_stream) do
    input_stream
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.to_integer/1)
    |> Enum.to_list()
  end
end

defmodule AdventOfCode.Day01.Part1 do
  @moduledoc """
  Day 1, Part 1
  """

  alias AdventOfCode.PuzzleSolver
  use PuzzleSolver

  import AdventOfCode.Day01, warn: false

  @impl PuzzleSolver
  def solve(input_stream) do
    depths = parse_input_stream(input_stream)

    Enum.zip_reduce(
      depths,
      Enum.drop(depths, 1),
      0,
      &if(&1 < &2, do: &3 + 1, else: &3)
    )
  end
end

defmodule AdventOfCode.Day01.Part2 do
  @moduledoc """
  Day 1, Part 2
  """

  alias AdventOfCode.PuzzleSolver
  use PuzzleSolver

  import AdventOfCode.Day01, warn: false

  @impl PuzzleSolver
  def solve(input_stream) do
    parse_input_stream(input_stream)
    |> window_increases()
  end

  defp window_increases(list, acc \\ 0)

  defp window_increases([a, b, c, d | _] = list, acc) do
    acc = if a + b + c < b + c + d, do: acc + 1, else: acc
    window_increases(Enum.drop(list, 1), acc)
  end

  defp window_increases(_, acc), do: acc
end
