defmodule AdventOfCode.Day01.Part1Test do
  use AdventOfCode.PuzzleCase, module: AdventOfCode.Day01.Part1

  test "returns :ok" do
    input = ~S"""
    input
    """

    assert_solution(input, "ok")
  end
end