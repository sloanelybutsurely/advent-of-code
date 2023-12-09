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
end
