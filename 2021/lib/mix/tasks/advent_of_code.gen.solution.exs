defmodule Mix.Tasks.AdventOfCode.Gen.Solution do
  use Mix.Task
  import Mix.Generator

  @shortdoc "Generate a new solution module"

  @moduledoc """
  #{@shortdoc}.

  Includes new solution module, test, and empty problem input.

  ## Examples

      # Generate solution modules, tests, and empty input for Day 2
      $ mix advent_of_code.gen.solution 2
  """

  @impl Mix.Task
  def run(_args) do
  end
end
