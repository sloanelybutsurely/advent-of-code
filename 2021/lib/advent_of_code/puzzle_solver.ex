defmodule AdventOfCode.PuzzleSolver do
  @moduledoc """
  Behaviour for a puzzle solution.
  """

  @doc """
  Given the input as a stream, return the solution as a string
  """
  @callback solve(IO.Stream.t()) :: String.t()

  defmacro __using__(_) do
    quote do
      @behaviour AdventOfCode.PuzzleSolver
    end
  end
end
