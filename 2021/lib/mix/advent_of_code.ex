defmodule Mix.AdventOfCode do
  @moduledoc """
  Helpers for `AdventOfCode` mix tasks.
  """

  def day_module(day) do
    Module.concat(AdventOfCode, Macro.camelize("Day#{day}"))
  end

  def part_module(day, part) do
    Module.concat(day_module(day), Macro.camelize("Part#{part}"))
  end
end
