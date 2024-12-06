import AOC

aoc 2024, 6 do
  defmodule State do
    @moduledoc false
    defstruct [:pos, :dir, grid: %{}, visited: MapSet.new(), visited_pos_and_dir: MapSet.new(), looped?: false]
  end

  def p1(input) do
    %State{} =
      final_state =
      input
      |> read_state()
      |> simulate()

    MapSet.size(final_state.visited)
  end

  def p2(input) do
    %State{} = state = read_state(input)

    in_front_of_guard = forward(state).pos

    for pos <- Map.keys(state.grid), pos != in_front_of_guard, not blocked?(state, pos), reduce: 0 do
      sum ->
        if (state
            |> insert_obstacle(pos)
            |> simulate()).looped? do
          sum + 1
        else
          sum
        end
    end
  end

  defp insert_obstacle(%State{grid: grid} = state, pos), do: %State{state | grid: Map.put(grid, pos, "#")}

  ## simulation

  defp simulate(%State{} = state) do
    state
    |> Stream.iterate(&step/1)
    |> Stream.drop_while(&(in_bounds?(&1) and not &1.looped?))
    |> Stream.take(1)
    |> Enum.at(0)
  end

  defp step(%State{} = state) do
    next = forward(state)

    cond do
      blocked?(next) ->
        rotate_90(state)

      MapSet.member?(state.visited_pos_and_dir, {next.pos, next.dir}) ->
        %State{state | looped?: true}

      true ->
        visit(next)
    end
  end

  defp in_bounds?(%State{grid: grid, pos: pos}), do: Map.has_key?(grid, pos)

  defp blocked?(%State{pos: pos} = state), do: blocked?(state, pos)
  defp blocked?(%State{grid: grid}, pos), do: Map.get(grid, pos) == "#"

  defp forward(%State{dir: :up, pos: {i, j}} = state), do: %State{state | pos: {i, j - 1}}
  defp forward(%State{dir: :down, pos: {i, j}} = state), do: %State{state | pos: {i, j + 1}}
  defp forward(%State{dir: :left, pos: {i, j}} = state), do: %State{state | pos: {i - 1, j}}
  defp forward(%State{dir: :right, pos: {i, j}} = state), do: %State{state | pos: {i + 1, j}}

  defp rotate_90(%State{dir: :up} = state), do: %State{state | dir: :right}
  defp rotate_90(%State{dir: :right} = state), do: %State{state | dir: :down}
  defp rotate_90(%State{dir: :down} = state), do: %State{state | dir: :left}
  defp rotate_90(%State{dir: :left} = state), do: %State{state | dir: :up}

  defp visit(%State{pos: pos, dir: dir, visited: visited, visited_pos_and_dir: visited_pos_and_dir} = state) do
    if in_bounds?(state) do
      %State{
        state
        | visited: MapSet.put(visited, pos),
          visited_pos_and_dir: MapSet.put(visited_pos_and_dir, {pos, dir})
      }
    else
      state
    end
  end

  ## input

  defp read_state(input) do
    lines =
      input
      |> String.split("\n")
      |> Enum.map(&String.graphemes/1)

    for {line, y} <- Enum.with_index(lines),
        {c, x} <- Enum.with_index(line),
        reduce: %State{} do
      %State{grid: grid, pos: nil, dir: nil} ->
        dir =
          case c do
            "^" -> :up
            ">" -> :right
            "<" -> :left
            "v" -> :down
            _ -> nil
          end

        if is_nil(dir) do
          %State{grid: Map.put(grid, {x, y}, c)}
        else
          %State{
            grid: Map.put(grid, {x, y}, "."),
            dir: dir,
            pos: {x, y},
            visited: MapSet.new([{x, y}])
          }
        end

      %State{grid: grid} = state ->
        %State{state | grid: Map.put(grid, {x, y}, c)}
    end
  end
end
