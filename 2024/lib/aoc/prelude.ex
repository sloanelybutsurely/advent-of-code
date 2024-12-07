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
end
