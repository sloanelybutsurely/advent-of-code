defmodule AdventOfCode.Day02.Part2Test do
  use AdventOfCode.PuzzleCase, module: AdventOfCode.Day02.Part2

  test "returns the product of horizontal position and depth" do
    input = ~S"""
    forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2
    """

    assert_solution(input, 900)
  end
end
