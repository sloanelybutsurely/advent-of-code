defmodule AOC.Prelude do
  @moduledoc """
  Commonly used functions
  """

  @doc """
  Splits a String input into lists of lines.

  Excludes any leading or trailing lines from the output list.
  """
  @spec lines(String.t()) :: [String.t()]
  def lines(str) do
    String.split(str, "\n", trim: true)
  end

  @doc """
  Takes a string containing multiple integers and returns a list of the parsed
  integers.

  The input is split on the given seperator. A single space is used if none is
  provided.
  """
  @spec ints(String.t()) :: [integer()]
  @spec ints(String.t(), separator :: String.t()) :: [integer()]
  def ints(str, separator \\ " ") do
    str
    |> String.split(separator, trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  @type map_grid(a) :: %{{x :: non_neg_integer(), y :: non_neg_integer()} => a}

  @doc """
  Reads an Advent of Code-style grid of characters into a map of x, y positions
  to a string containing the single character at that position.
  """
  @spec map_grid(String.t()) :: map_grid(String.t())
  def map_grid(input) do
    for {line, y} <- Enum.with_index(lines(input)), {c, x} <- Enum.with_index(String.graphemes(line)), into: %{} do
      {{x, y}, c}
    end
  end

  @doc """
  Returns `true` if the given position tuple is within the bounds of the
  map_grid.
  """
  @spec in_bounds?(map_grid(any()), {x :: integer(), y :: integer()}) :: boolean()
  def in_bounds?(grid, pos), do: Map.has_key?(grid, pos)
end
