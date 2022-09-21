import AOC

aoc 2021, 12 do
  @target "end"

  def input_graph() do
    input_stream()
    |> Enum.reduce(Graph.new(type: :undirected), fn line, graph ->
      [l, r] = String.split(line, "-", trim: true)
      Graph.add_edge(graph, l, r)
    end)
  end

  def all_paths(graph, can_visit?, curr \\ "start", path \\ ["start"])
  def all_paths(_graph, _can_visit?, @target, path), do: [path]

  def all_paths(graph, can_visit?, curr, path) do
    neighbors = Graph.neighbors(graph, curr)

    if Enum.empty?(neighbors) do
      []
    else
      neighbors
      |> Enum.flat_map(fn neighbor ->
        if can_visit?.(neighbor, path) do
          all_paths(graph, can_visit?, neighbor, [neighbor | path])
        else
          []
        end
      end)
    end
  end

  def cave_is_big?(cave), do: String.upcase(cave) == cave

  def p1 do
    input_graph()
    |> all_paths(fn cave, path -> cave_is_big?(cave) or cave not in path end)
    |> length()
  end

  def p2 do
    input_graph()
    |> all_paths(fn cave, path ->
      if cave_is_big?(cave) do
        true
      else
        if cave not in path do
          true
        else
          if cave in ["start", "end"] do
            false
          else
            max_small_cave_in_path =
              path
              |> Enum.reject(&cave_is_big?/1)
              |> Enum.frequencies()
              |> Map.values()
              |> Enum.max()

            max_small_cave_in_path < 2
          end
        end
      end
    end)
    |> length()
  end
end
