defmodule AdventOfCodeTest do
  use ExUnit.Case

  import AdventOfCode

  describe "solver/1" do
    test "returns a module for a solver" do
      assert solver("1.1") == AdventOfCode.Day01.Part1
      assert solver("2.2") == AdventOfCode.Day02.Part2
      assert solver("3.1") == AdventOfCode.Day03.Part1
    end

    test "returns a solver module for zero padded numbers" do
      assert solver("01.01") == AdventOfCode.Day01.Part1
      assert solver("02.2") == AdventOfCode.Day02.Part2
      assert solver("3.01") == AdventOfCode.Day03.Part1
      assert solver("12.1") == AdventOfCode.Day12.Part1
      assert solver("24.02") == AdventOfCode.Day24.Part2
    end
  end
end
