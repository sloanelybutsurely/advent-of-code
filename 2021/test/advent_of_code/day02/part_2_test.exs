defmodule AdventOfCode.Day02.Part2Test do
  use AdventOfCode.PuzzleCase, module: AdventOfCode.Day02.Part2

  test "returns :ok" do
    input = ~S"""
    input
    """

    assert_solution(input, "ok")
  end
end