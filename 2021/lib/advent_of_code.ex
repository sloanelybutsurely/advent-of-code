defmodule AdventOfCode do
  @moduledoc """
  Solutions to the 2021 Advent of Code puzzles
  """

  def solver(selector) do
    [day, part] = String.split(selector, ".")
    part_module(day, part)
  end

  def part_module(day, part) do
    Module.concat(day_module(day), Macro.camelize("Part#{part_number(part)}"))
  end

  def day_module(day) do
    Module.concat(AdventOfCode, Macro.camelize("Day#{day_number(day)}"))
  end

  defp day_number(day), do: String.pad_leading(day, 2, "0")
  defp part_number(part), do: String.trim_leading(part, "0")
end
