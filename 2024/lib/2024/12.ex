import AOC
import AOC.Prelude

aoc 2024, 12 do
  def p1(input) do
    input
    |> map_grid()
    |> group_plots()
    |> Enum.map(&size_plot/1)
    |> Enum.map(fn {area, perimeter, _} -> area * perimeter end)
    |> Enum.sum()
  end

  def p2(input) do
    input
    |> map_grid()
    |> group_plots()
    |> Enum.map(&size_plot/1)
    |> Enum.map(fn {area, _, sides} -> area * sides end)
    |> Enum.sum()
  end

  ## group plots

  defp group_plots(map, plots \\ [])
  defp group_plots(map, plots) when map_size(map) == 0, do: plots

  defp group_plots(map, plots) do
    {pos, plant} = Enum.at(map, 0)

    plot = walk_plot(map, pos, plant)

    {_, map} = Map.split(map, MapSet.to_list(plot))

    group_plots(map, [plot | plots])
  end

  defp size_plot(plot) do
    area = MapSet.size(plot)

    perimeter =
      for pos <- plot, reduce: 0 do
        sum ->
          sum +
            ([&n/1, &s/1, &e/1, &w/1]
             |> Enum.map(& &1.(pos))
             |> Enum.count(&(not MapSet.member?(plot, &1))))
      end

    {area, perimeter, count_sides(plot)}
  end

  ## traversal

  defp walk_plot(map, pos, plant, plot \\ MapSet.new()) do
    plot = MapSet.put(plot, pos)

    [&n/1, &s/1, &e/1, &w/1]
    |> Enum.map(& &1.(pos))
    |> Enum.reject(&MapSet.member?(plot, &1))
    |> Enum.filter(&(Map.get(map, &1) == plant))
    |> Enum.reduce(plot, fn pos, plot ->
      walk_plot(map, pos, plant, plot)
    end)
  end

  def n({x, y}), do: {x, y + 1}
  def s({x, y}), do: {x, y - 1}
  def e({x, y}), do: {x + 1, y}
  def w({x, y}), do: {x - 1, y}

  defp count_sides(plot) do
    # find exterior faces
    exterior_faces =
      for pos <- plot, dir <- ~w[n e s w]a, exterior?(plot, pos, dir) do
        {pos, dir}
      end

    exterior_faces
    # group exterior faces
    |> Enum.group_by(
      fn
        {{_, y}, dir} when dir in ~w[n s]a -> {dir, y}
        {{x, _}, dir} when dir in ~w[e w]a -> {dir, x}
      end,
      fn
        {{x, _}, dir} when dir in ~w[n s]a -> x
        {{_, y}, dir} when dir in ~w[e w]a -> y
      end
    )
    |> Map.values()
    # count groups of exterior faces, splitting non-monotonic sections
    |> Enum.map(fn group ->
      {faces, _} =
        group
        |> Enum.sort()
        |> Enum.reduce({0, nil}, fn
          n, {sides, nil} -> {sides + 1, n}
          n, {sides, last} when last + 1 == n -> {sides, n}
          n, {sides, _last} -> {sides + 1, n}
        end)

      faces
    end)
    |> Enum.sum()
  end

  defp exterior?(plot, pos, dir) do
    neighboring_pos = apply(__MODULE__, dir, [pos])
    not MapSet.member?(plot, neighboring_pos)
  end
end
