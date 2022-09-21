defmodule Mix.AdventOfCode do
  @moduledoc """
  Helpers for `AdventOfCode` mix tasks.
  """

  defdelegate day_module(day), to: AdventOfCode
  defdelegate part_module(day, part), to: AdventOfCode
end
