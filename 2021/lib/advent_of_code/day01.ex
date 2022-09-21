defmodule AdventOfCode.Day01 do
  @moduledoc """
  Day 1
  """

  def parse_input_stream(input_stream) do
    input_stream
    |> Stream.map(&String.trim/1)
    |> Stream.map(&String.to_integer/1)
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
    depth_stream =
      input_stream
      |> parse_input_stream()
      |> Enum.to_list()

    Enum.zip_with(
      depth_stream,
      Enum.drop(depth_stream, 1),
      &</2
    )
    |> Stream.filter(&is_true?/1)
    |> Enum.count()
  end

  defp is_true?(true), do: true
  defp is_true?(_), do: false
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
    |> Enum.to_list()
    |> window_increases()
  end

  defp window_increases(list, acc \\ 0)

  defp window_increases([a, b, c, d | _] = list, acc) do
    acc = if a + b + c < b + c + d, do: acc + 1, else: acc
    window_increases(Enum.drop(list, 1), acc)
  end

  defp window_increases(_, acc), do: acc
end
