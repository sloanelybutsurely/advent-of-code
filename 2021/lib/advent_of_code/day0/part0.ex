defmodule AdventOfCode.Day0.Part0 do
  alias AdventOfCode.PuzzleSolver

  use PuzzleSolver

  @impl PuzzleSolver
  def solve(stream) do
    Stream.run(stream)
    "42"
  end
end
