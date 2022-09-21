defmodule AdventOfCode.Day01.Part1Test do
  use AdventOfCode.PuzzleCase, module: AdventOfCode.Day01.Part1

  test "solves for a small input" do
    input = ~S"""
    199
    200
    208
    210
    200
    207
    240
    269
    260
    263
    """

    assert_solution(input, 7)
  end
end
