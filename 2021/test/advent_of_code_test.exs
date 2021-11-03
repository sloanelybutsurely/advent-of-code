defmodule AdventOfCodeTest do
  use ExUnit.Case

  import AdventOfCode

  describe "solver/1" do
    test "returns a module for a solver" do
      assert solver("1.1") == AdventOfCode.Day1.Part1
      assert solver("2.2") == AdventOfCode.Day2.Part2
      assert solver("3.1") == AdventOfCode.Day3.Part1
    end
  end
end
