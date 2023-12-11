import AOC
# import AOCHelpers
# alias AOCHelpers.Grid

aoc 2023, 10 do
  def p1(input) do
    # grid = Grid.to_grid(input)

    # start = Grid.find_value(grid, "S")

    # longest_in_cycle(grid, start)
  end

  def p2(_input) do
  end

  # def find_cycle(grid, start), do: find_cycle(grid, start, start)
  # def find_cycle(grid, pos, start, path \\ []) do
  #   path = [pos | path]

  #   neighbors =
  #     grid
  #     |> Grid.neighbors(pos)
  #     |> Enum.filter(&connects?/1)
  #     |> Enum.reject(fn {coords, _, _} -> coords in path end)
  #     |> Enum.map(&elem(&1, 0))

  #   case neighbors

  # end

  # def longest_in_cycle(grid, pos, seen \\ %{}, dist \\ 0) do
  #   seen = Map.update(seen, pos, dist, &min(&1, dist))

  #   neighbors =
  #     grid
  #     |> Grid.neighbors(pos)
  #     |> Enum.filter(&connects?/1)
  #     |> Enum.reject(&seen_shorter?(seen, &1, dist + 1))

  #   case neighbors do
  #     [] ->
  #       {pos, seen, dist}

  #     neighbors ->
  #       {visited, seen} =
  #         neighbors
  #         |> Enum.map_reduce(seen, fn {neighbor, _, _}, seen ->
  #           {_, seen, _} = result = longest_in_cycle(grid, neighbor, seen, dist + 1)

  #           {result, seen}
  #         end)

  #       {pos, _, dist} = Enum.max_by(visited, &elem(&1, 2))
  #       {pos, seen, dist}
  #   end
  # end

  # def connects?({_, [:north], p}) when p in ~w[| 7 F], do: true
  # def connects?({_, [:east], p}) when p in ~w[- J 7], do: true
  # def connects?({_, [:south], p}) when p in ~w[| L J], do: true
  # def connects?({_, [:west], p}) when p in ~w[- L F], do: true
  # def connects?(_), do: false

  # def seen_shorter?(seen, {coords, _, _}, dist), do: seen_shorter?(seen, coords, dist)

  # def seen_shorter?(seen, coords, dist) do
  #   case Map.get(seen, coords) do
  #     nil -> false
  #     n -> n <= dist
  #   end
  # end

  # def print_grid(grid) do
  #   grid
  #   |> Grid.map(fn
  #     "." -> " "
  #     "L" -> "└"
  #     "J" -> "┘"
  #     "7" -> "┐"
  #     "F" -> "┌"
  #     "-" -> "─"
  #     n -> n
  #   end)
  #   |> Grid.inspect()
  # end
end
