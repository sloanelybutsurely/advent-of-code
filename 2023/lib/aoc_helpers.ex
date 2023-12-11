defmodule AOCHelpers do
  def lines(string) when is_binary(string) do
    String.split(string, "\n")
  end

  def words(str) when is_binary(str) do
    String.split(str)
  end

  def letters(str) when is_binary(str) do
    String.split(str, "", trim: true)
  end

  def integers(str) when is_binary(str) do
    str
    |> words()
    |> integers()
  end

  def integers(ws) when is_list(ws) do
    Enum.map(ws, &String.to_integer/1)
  end

  def lines_of_integers(input) do
    input
    |> lines()
    |> Enum.map(&integers/1)
  end

  def to_grid(str) do
    lists =
      str
      |> lines()
      |> Enum.map(&letters/1)

    map =
      for {list, y} <- Enum.with_index(lists), {v, x} <- Enum.with_index(list), into: %{} do
        {{x, y}, v}
      end

    max_x =
      map
      |> Enum.map(fn {{x, _}, _} -> x end)
      |> Enum.max()

    max_y =
      map
      |> Enum.map(fn {{_, y}, _} -> y end)
      |> Enum.max()

    bounds = {0..max_x, 0..max_y}

    {map, bounds}
  end

  @doc """
  Take a list of terms and a list of 1-arity functions and apply each function
  to the coresponding term in the list of terms.

  ## Example

      iex> map_list([1, 2, 3], [&(&1 + 1), &(&1 * 100), &to_string/1])
      [2, 200, "3"]

  """
  def map_list(args, funs) do
    Enum.zip_with(args, funs, fn arg, fun -> fun.(arg) end)
  end

  def id(x), do: x
  def always(x), do: fn -> x end
  def is?(x), do: &(&1 == x)

  def combinations(_, 0), do: [[]]
  def combinations([], _), do: []

  def combinations([x | xs], n) do
    for(tail <- combinations(xs, n - 1), do: [x | tail]) ++ combinations(xs, n)
  end
end
