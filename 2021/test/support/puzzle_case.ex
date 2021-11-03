defmodule AdventOfCode.PuzzleCase do
  @moduledoc """
  Defines tests for an `AdventOfCode.PuzzleSolver` module.
  """

  use ExUnit.CaseTemplate

  using module: module do
    quote bind_quoted: [module: module] do
      @module module

      defp assert_solution(input, desired_output) when is_binary(input) do
        {:ok, stream_pid} = StringIO.open(input)
        stream_input = IO.stream(stream_pid, :line)
        actual_output = AdventOfCode.PuzzleSolver.solve(@module, stream_input)
        @module.solve(stream_input)
        assert actual_output == desired_output
      end
    end
  end
end
