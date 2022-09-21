defmodule AdventOfCode.Day02 do
  @moduledoc """
  Day 2
  """

  def parse_action("forward " <> v), do: {:forward, String.to_integer(v)}
  def parse_action("down " <> v), do: {:down, String.to_integer(v)}
  def parse_action("up " <> v), do: {:up, String.to_integer(v)}
end

defmodule AdventOfCode.Day02.Part1 do
  @moduledoc """
  Day 2, Part 1
  """

  alias AdventOfCode.PuzzleSolver
  use PuzzleSolver

  import AdventOfCode.Day02, warn: false

  @impl PuzzleSolver
  def solve(input_stream) do
    input_stream
    |> Stream.map(&String.trim/1)
    |> Stream.map(&parse_action/1)
    |> Enum.reduce({0, 0}, &apply_action/2)
    |> product()
  end

  defp apply_action({:forward, v}, {horiz, depth}), do: {horiz + v, depth}
  defp apply_action({:down, v}, {horiz, depth}), do: {horiz, depth + v}
  defp apply_action({:up, v}, {horiz, depth}), do: {horiz, depth - v}

  defp product({x, y}), do: x * y
end

defmodule AdventOfCode.Day02.Part2 do
  @moduledoc """
  Day 2, Part 2
  """

  alias AdventOfCode.PuzzleSolver
  use PuzzleSolver

  import AdventOfCode.Day02, warn: false

  @impl PuzzleSolver
  def solve(input_stream) do
    input_stream
    |> Stream.map(&String.trim/1)
    |> Stream.map(&parse_action/1)
    |> Enum.reduce({0, 0, 0}, &apply_action/2)
    |> product()
  end

  defp apply_action({:forward, v}, {horiz, depth, aim}), do: {horiz + v, depth + aim * v, aim}
  defp apply_action({:down, v}, {horiz, depth, aim}), do: {horiz, depth, aim + v}
  defp apply_action({:up, v}, {horiz, depth, aim}), do: {horiz, depth, aim - v}

  defp product({x, y, _z}), do: x * y
end

