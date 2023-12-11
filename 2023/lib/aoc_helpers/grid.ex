defmodule AOCHelpers.Grid do
  import AOCHelpers

  defstruct [:map, :bounds]

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

    %__MODULE__{map: map, bounds: bounds}
  end

  def in_bounds?(%__MODULE__{bounds: {min_x..max_x, min_y..max_y}}, {x, y}) do
    min_x <= x and x <= max_x and min_y <= y and y <= max_y
  end

  def update!(grid, coords, value) do
    %__MODULE__{grid | map: Map.update!(grid.map, coords, value)}
  end

  def get(grid, coords, default \\ nil) do
    Map.get(grid.map, coords, default)
  end

  def map(grid, fun) do
    map =
      Enum.map(grid.map, fn {coords, value} ->
        {coords, fun.(value)}
      end)
      |> Enum.into(%{})

    %__MODULE__{grid | map: map}
  end

  def inspect(%__MODULE__{map: map, bounds: bounds}) do
    {xs, ys} = bounds

    for y <- ys, into: "" do
      line =
        for x <- xs, into: "" do
          Map.get(map, {x, y}, " ")
        end

      "#{line}\n"
    end
    |> IO.puts()
  end

  def find_value(grid, value) do
    with {coords, _} <- Enum.find(grid.map, fn {_, v} -> v == value end) do
      coords
    end
  end

  def find(grid, fun) do
    Enum.find(grid.map, fn {_, v} -> fun.(v) end)
  end

  def neighbors(grid, coords, diag? \\ false) do
    coords
    |> neighbor_coords()
    |> Enum.filter(fn {c, dirs} ->
      if diag? do
        in_bounds?(grid, c)
      else
        in_bounds?(grid, c) and length(dirs) == 1
      end
    end)
    |> Enum.map(fn {c, dirs} ->
      {c, dirs, Map.get(grid.map, c)}
    end)
  end

  defp neighbor_coords({x, y}) do
    for {j, j_dir} <- [{y - 1, [:north]}, {y, []}, {y + 1, [:south]}],
        {i, i_dir} <- [{x - 1, [:west]}, {x, []}, {x + 1, [:east]}],
        {x, y} != {i, j} do
      {{i, j}, j_dir ++ i_dir}
    end
  end
end

# defimpl Inspect, for: AOCHelpers.Grid do
#   import Inspect.Algebra

#   def inspect(grid, opts) do

#   end
# end
