import AOC

defmodule PriorityQueue2 do
  defstruct [:p_queue, :map_set]

  def new() do
    %__MODULE__{
      p_queue: PriorityQueue.new(),
      map_set: MapSet.new()
    }
  end

  def push(%__MODULE__{p_queue: p_queue, map_set: map_set} = p_queue_2, key, weight) do
    if MapSet.member?(map_set, key) do
      p_queue_2
    else
      %__MODULE__{
        p_queue: PriorityQueue.push(p_queue, key, weight),
        map_set: MapSet.put(map_set, key)
      }
    end
  end

  def pop(%__MODULE__{p_queue: p_queue, map_set: map_set} = p_queue_2) do
    case PriorityQueue.pop(p_queue) do
      {{:value, key} = result, p_queue} ->
        {result,
         %__MODULE__{
           p_queue: p_queue,
           map_set: MapSet.delete(map_set, key)
         }}

      {:empty = result, _} ->
        {result, p_queue_2}
    end
  end
end

aoc 2021, 15 do
  use Timex

  def input(input_string \\ input_string()) do
    lists =
      input_string
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.graphemes()
        |> Enum.map(&String.to_integer/1)
      end)

    for {row, i} <- Enum.with_index(lists, 1), {cell, j} <- Enum.with_index(row, 1), into: %{} do
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
      PriorityQueue2.new()
      |> PriorityQueue2.push(start, 0)

    g_scores = %{start => 0}
    f_scores = %{start => manhattan_dist(start, goal)}

    a_star(map, goal, open, g_scores, f_scores)
  end

  def a_star(map, goal, open, g_scores, f_scores) do
    case PriorityQueue2.pop(open) do
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

                  open = PriorityQueue2.push(open, neighbor, f_scores[neighbor])

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

  # This can totally be done with math oh well
  def clamp_weight(n) do
    Stream.cycle(1..9)
    |> Stream.drop(n - 1)
    |> Enum.take(1)
    |> hd()
  end

  def expand_map(tile, n) do
    {tile_x_size, tile_y_size} = tile |> Map.keys() |> Enum.max()

    for i <- 0..(n - 1), j <- 0..(n - 1), reduce: %{} do
      map ->
        Map.merge(
          map,
          for {{x, y}, w} <- tile, into: %{} do
            {{tile_x_size * i + x, tile_y_size * j + y}, clamp_weight(w + i + j)}
          end
        )
    end
  end

  def lowest_total_risk_to_end(map) do
    start = {1, 1}
    goal = map |> Map.keys() |> Enum.max()

    {duration, result} =
      Duration.measure(fn ->
        a_star(map, start, goal)
      end)

    duration = Timex.format_duration(duration, :humanized)

    "Result #{result} found in #{duration}"
  end

  def p1 do
    input()
    |> lowest_total_risk_to_end()
  end

  def p2 do
    input()
    |> expand_map(5)
    |> lowest_total_risk_to_end()
  end
end
