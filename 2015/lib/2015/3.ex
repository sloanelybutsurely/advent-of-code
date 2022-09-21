import AOC

aoc 2015, 3 do
  def input_chars(), do: input_string() |> String.split("", trim: true)

  def p1 do
    input_chars()
    |> run()
    |> then(fn {visited, _} -> MapSet.size(visited) end)
  end

  def p2 do
    {santa, robo_santa} =
      input_chars()
      |> Enum.with_index()
      |> Enum.split_with(fn {_, i} -> Integer.mod(i, 2) == 0 end)
      |> then(fn {santa, robo_santa} ->
        {without_index(santa), without_index(robo_santa)}
      end)

    {visited, _} = run(santa)
    {visited, _} = run(robo_santa, visited)

    MapSet.size(visited)
  end

  defp op("^"), do: {1, 0}
  defp op(">"), do: {0, 1}
  defp op("v"), do: {-1, 0}
  defp op("<"), do: {0, -1}

  defp add({x, y}, {a, b}), do: {x + a, y + b}

  defp run(inst, visited \\ MapSet.new()) do
    starting_position = {0, 0}
    visited = MapSet.put(visited, starting_position)

    Enum.reduce(inst, {visited, starting_position}, fn command, {visited, current_position} ->
      current_position = add(current_position, op(command))
      visited = MapSet.put(visited, current_position)
      {visited, current_position}
    end)
  end

  defp without_index(enum), do: Enum.map(enum, &elem(&1, 0))
end
