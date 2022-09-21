import AOC

aoc 2015, 9 do
  def parse_line(line) do
    [from, _, to, _, distance] = String.split(line, " ")
    {from, to, String.to_integer(distance)}
  end

  def p1 do
    graph =
      input_stream()
      |> Stream.map(&parse_line/1)
      |> Enum.reduce(Graph.new(type: :undirected), fn {v1, v2, weight}, graph ->
        Graph.add_edge(graph, v1, v2, weight: weight)
      end)

    for start <- Graph.vertices(graph), reduce: :infinity do
      shortest_so_far -> find_shortest_path_to_all_vertices(graph, start) |> min(shortest_so_far)
    end
  end

  def find_shortest_path_to_all_vertices(graph, start, distance \\ 0)

  def find_shortest_path_to_all_vertices(%{vertices: v}, _, distance) when map_size(v) == 1,
    do: distance

  def find_shortest_path_to_all_vertices(graph, start, distance) do
    for neighbor <- Graph.neighbors(graph, start), reduce: :infinity do
      shortest_path_distance ->
        %{weight: distance_to_neighbor} = Graph.edge(graph, start, neighbor)

        find_shortest_path_to_all_vertices(
          Graph.delete_vertex(graph, start),
          neighbor,
          distance_to_neighbor + distance
        )
        |> min(shortest_path_distance)
    end
  end

  def p2 do
    graph =
      input_stream()
      |> Stream.map(&parse_line/1)
      |> Enum.reduce(Graph.new(type: :undirected), fn {v1, v2, weight}, graph ->
        Graph.add_edge(graph, v1, v2, weight: weight)
      end)

    for start <- Graph.vertices(graph), reduce: 0 do
      longest_so_far -> find_longest_path_to_all_vertices(graph, start) |> max(longest_so_far)
    end
  end

  def find_longest_path_to_all_vertices(graph, start, distance \\ 0)

  def find_longest_path_to_all_vertices(%{vertices: v}, _, distance) when map_size(v) == 1,
    do: distance

  def find_longest_path_to_all_vertices(graph, start, distance) do
    for neighbor <- Graph.neighbors(graph, start), reduce: 0 do
      longest_path_distance ->
        %{weight: distance_to_neighbor} = Graph.edge(graph, start, neighbor)

        find_longest_path_to_all_vertices(
          Graph.delete_vertex(graph, start),
          neighbor,
          distance_to_neighbor + distance
        )
        |> max(longest_path_distance)
    end
  end
end
