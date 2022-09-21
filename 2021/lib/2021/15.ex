import AOC

aoc 2021, 15 do
  def input(input_string \\ input_string()) do
    lists =
      input_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.graphemes()
        |> Enum.map(&String.to_integer/1)
      end)

    for {row, i} <- Enum.with_index(lists), {cell, j} <- Enum.with_index(row), into: %{} do
      {{i, j}, cell}
    end
  end

  def move({i, j}, {g, h}), do: {g.(i), h.(j)}

  def neighbors(map, pos) do
    [
      {&(&1 + 1), & &1},
      {& &1, &(&1 + 1)},
      {&(&1 - 1), & &1},
      {& &1, &(&1 - 1)}
    ]
    |> Enum.map(&move(pos, &1))
    |> Enum.filter(&Map.has_key?(map, &1))
  end

  def manhattan_dist({x1, y1}, {x2, y2}), do: abs(x1 - x2) + abs(y1 - y2)

  def a_star(map, start, goal) do
    open =
      PriorityQueue.new()
      |> PriorityQueue.push(start, 0)

    g_scores = %{start => 0}
    f_scores = %{start => manhattan_dist(start, goal)}

    a_star(map, goal, open, g_scores, f_scores)
  end

  # Gets into PriorityQueue implementation details and is slow...
  # https://github.com/bitwalker/libgraph/blob/46c435f507f8b99f3f14c96d380eda28e5e9c7de/lib/priority_queue.ex#L129-L132
  def p_queue_member?(%PriorityQueue{priorities: tree}, v) do
    v in (:gb_trees.to_list(tree) |> Enum.flat_map(fn {_, q} -> :queue.to_list(q) end))
  end

  def a_star(map, goal, open, g_scores, f_scores) do
    case PriorityQueue.pop(open) do
      {{:value, curr}, open} ->
        if curr == goal do
          f_scores[curr]
        else
          {open, g_scores, f_scores} =
            for neighbor <- neighbors(map, curr), reduce: {open, g_scores, f_scores} do
              {open, g_scores, f_scores} ->
                tentative_g_score = g_scores[curr] + Map.get(map, neighbor)

                if tentative_g_score < g_scores[neighbor] do
                  g_scores = Map.put(g_scores, neighbor, tentative_g_score)

                  f_scores =
                    Map.put(
                      f_scores,
                      neighbor,
                      tentative_g_score + manhattan_dist(neighbor, goal)
                    )

                  open =
                    if p_queue_member?(open, neighbor),
                      do: open,
                      else: PriorityQueue.push(open, neighbor, f_scores[neighbor])

                  {open, g_scores, f_scores}
                else
                  {open, g_scores, f_scores}
                end
            end

          a_star(map, goal, open, g_scores, f_scores)
        end

      {:empty, _} ->
        :error
    end
  end

  def p1 do
    map = input()

    start = {0, 0}
    goal = map |> Map.keys() |> Enum.max()

    a_star(map, start, goal)
  end

  def p2 do
  end
end
