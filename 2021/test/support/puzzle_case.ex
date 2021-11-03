defmodule AdventOfCode.PuzzleCase do
  @moduledoc """
  Defines tests for an `AdventOfCode.PuzzleSolver` module.
  """

  use ExUnit.CaseTemplate

  using module: module do
    quote bind_quoted: [module: module] do
      @module module

      defp assert_solution(input, desired_output) do
        actual_output = @module.run(input)
        assert actual_output == desired_output
      end
    end
  end
end
