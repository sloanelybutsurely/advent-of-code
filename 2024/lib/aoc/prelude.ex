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

  @type coord() :: {x :: integer(), y :: integer()}
  @type coord(t) :: {x :: t, y :: t}
  @type map_grid(a) :: %{coord(non_neg_integer()) => a}

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
  @spec in_bounds?(map_grid(any()), coord()) :: boolean()
  def in_bounds?(grid, pos), do: Map.has_key?(grid, pos)

  @type direction() :: :up | :down | :left | :right | :n | :ne | :e | :se | :s | :sw | :w | :nw

  @doc """
  Applies a single movement to a coordindate pair

  There are also single function shorthands: 
  """
  @spec move_coord(coord(), direction()) :: coord()
  def move_coord({x, y}, dir) when dir in [:up, :n], do: {x, y - 1}
  def move_coord({x, y}, dir) when dir in [:down, :s], do: {x, y + 1}
  def move_coord({x, y}, dir) when dir in [:left, :w], do: {x - 1, y}
  def move_coord({x, y}, dir) when dir in [:right, :e], do: {x + 1, y}
  def move_coord(pos, :ne), do: Enum.reduce([:n, :e], pos, &move_coord/2)
  def move_coord(pos, :se), do: Enum.reduce([:s, :e], pos, &move_coord/2)
  def move_coord(pos, :sw), do: Enum.reduce([:s, :w], pos, &move_coord/2)
  def move_coord(pos, :nw), do: Enum.reduce([:n, :w], pos, &move_coord/2)

  directions = ~w[up down left right n ne e se s sw w nw]a

  for dir <- directions do
    @doc """
    A shorthand for `move_coord(pos, #{inspect(dir)})`
    """
    @spec unquote(dir)(coord()) :: coord()
    def unquote(dir)(coord), do: move_coord(coord, unquote(dir))
  end

  @type neighborhood_opt() :: {:include_center, boolean()}
  @type neighborhood_opts() :: list(neighborhood_opt())

  @doc """
  For a given coordinate position, returns the [Von Neumann neighborhood][1].

  The Von Neumann neighborhood consists of the cells to the north, south, east,
  and west of the center cell.

  ## Options

  - `:include_center`, whether or not to include the passed position. Defaults
     to `false`

  [1]: https://en.wikipedia.org/wiki/Von_Neumann_neighborhood
  """
  @spec von_neumann_neighborhood(coord()) :: nonempty_list(coord())
  @spec von_neumann_neighborhood(coord(), opts :: neighborhood_opts()) :: nonempty_list(coord())
  def von_neumann_neighborhood(pos, opts \\ []) do
    include_center = Keyword.get(opts, :include_center, false)
    cells = Enum.map(~w[n e s w]a, &move_coord(pos, &1))
    maybe_center = if include_center, do: [pos], else: []
    maybe_center ++ cells
  end

  @doc """
  For a given coordinate position, returns the [Moore neighborhood][1].

  The Moore neighborhood consists of the cells to the north, northeast, east,
  southeast, south, southwest, west, and northwest of the center cell.

  ## Options

  - `:include_center`, whether or not to include the passed position. Defaults
     to `false`

  [1]: https://en.wikipedia.org/wiki/Moore_neighborhood
  """
  def moore_neighborhood(pos, opts \\ []) do
    include_center = Keyword.get(opts, :include_center, false)
    cells = Enum.map(~w[n ne e se s sw w nw]a, &move_coord(pos, &1))
    maybe_center = if include_center, do: [pos], else: []
    maybe_center ++ cells
  end
end
