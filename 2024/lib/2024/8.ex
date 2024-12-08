import AOC
import AOC.Prelude

aoc 2024, 8 do
  def p1(input) do
    input
    |> map_grid()
    |> count_antinodes(&antinodes_for_tower_pair/1)
  end

  def p2(input) do
    grid = map_grid(input)

    count_antinodes(grid, &resonant_harmonics_antinodes_for_tower_pair(&1, grid))
  end

  ## 

  defp count_antinodes(grid, antinodes_for_tower_fun) do
    grid
    |> find_tower_pairs()
    |> Enum.map(&elem(&1, 0))
    |> Enum.flat_map(antinodes_for_tower_fun)
    |> Enum.filter(&in_bounds?(grid, &1))
    |> Enum.uniq()
    |> Enum.count()
  end

  defp find_tower_pairs(grid) do
    tower_locaions = Map.reject(grid, &match?({_, "."}, &1))

    for {pos_a, t} <- tower_locaions, {pos_b, ^t} <- tower_locaions, pos_a != pos_b, into: %{} do
      {[pos_a, pos_b]
       |> Enum.sort()
       |> List.to_tuple(), t}
    end
  end

  defp antinodes_for_tower_pair({pos_a, pos_b}) do
    d = delta(pos_a, pos_b)

    [sub_delta(pos_a, d), add_delta(pos_b, d)]
  end

  defp resonant_harmonics_antinodes_for_tower_pair({pos_a, pos_b}, grid) do
    d = delta(pos_a, pos_b)

    sub_stream = pos_a |> Stream.iterate(&sub_delta(&1, d)) |> Stream.take_while(&in_bounds?(grid, &1))
    add_stream = pos_b |> Stream.iterate(&add_delta(&1, d)) |> Stream.take_while(&in_bounds?(grid, &1))

    sub_stream
    |> Stream.concat(add_stream)
    |> Enum.to_list()
  end

  defp delta({x1, y1}, {x2, y2}), do: {x2 - x1, y2 - y1}
  defp add_delta({x, y}, {xd, yd}), do: {x + xd, y + yd}
  defp sub_delta({x, y}, {xd, yd}), do: {x - xd, y - yd}
end
