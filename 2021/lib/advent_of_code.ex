defmodule AdventOfCode do
  @moduledoc """
  Solutions to the 2021 Advent of Code puzzles
  """

  def solver(selector) do
    [day, part] = String.split(selector, ".")
    day_module = Macro.camelize("Day#{day}")
    part_module = Macro.camelize("Part#{part}")
    Module.concat([AdventOfCode, day_module, part_module])
  end
end
