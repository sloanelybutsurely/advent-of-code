import AOC

aoc 2021, 11 do
  use AOCHelpers
  @h 10
  @w 10

  def input() do
    lists =
      input_string()
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.split("", trim: true)
        |> Enum.map(&String.to_integer/1)
      end)

    map =
      for {row, i} <- Enum.with_index(lists, 1), {cell, j} <- Enum.with_index(row, 1) do
        {{i, j}, cell}
      end
      |> Map.new()

    map
  end

  def n({i, j}), do: {i - 1, j}
  def e({i, j}), do: {i, j + 1}
  def s({i, j}), do: {i + 1, j}
  def w({i, j}), do: {i, j - 1}
  def ne(pos), do: pos |> n() |> e()
  def se(pos), do: pos |> s() |> e()
  def sw(pos), do: pos |> s() |> w()
  def nw(pos), do: pos |> n() |> w()

  def neighbor_positions(map, pos) do
    [&n/1, &ne/1, &e/1, &se/1, &s/1, &sw/1, &w/1, &nw/1]
    |> Enum.map(&apply(&1, [pos]))
    |> Enum.filter(&Map.has_key?(map, &1))
  end

  def neighbor_cells(map, pos) do
    neighbor_positions(map, pos)
    |> Enum.map(&Map.get(map, &1))
  end

  def tick(map) do
    for {pos, cell} <- map, reduce: map do
      acc -> Map.put(acc, pos, cell + 1)
    end
    |> simulate()
    |> then(fn {map, flashes} ->
      map =
        for {pos, cell} <- map, reduce: map do
          map ->
            if cell > 9 do
              Map.put(map, pos, 0)
            else
              map
            end
        end

      {map, flashes}
    end)
  end

  def simulate(map) do
    flashed =
      Enum.filter(map, fn {_, cell} -> cell > 9 end)
      |> Enum.map(&elem(&1, 0))
      |> MapSet.new()

    {map, flashed} =
      for pos <- flashed, neighbor <- neighbor_positions(map, pos), reduce: {map, flashed} do
        {map, flashed} -> increase_power_level(map, flashed, neighbor)
      end

    {map, MapSet.size(flashed)}
  end

  def increase_power_level(map, flashed, pos) do
    cell = Map.get(map, pos) + 1
    map = Map.put(map, pos, cell)

    if cell > 9 and not MapSet.member?(flashed, pos) do
      flashed = MapSet.put(flashed, pos)

      for neighbor <- neighbor_positions(map, pos), reduce: {map, flashed} do
        {map, flashed} -> increase_power_level(map, flashed, neighbor)
      end
    else
      {map, flashed}
    end
  end

  def render_cell(0), do: " "
  def render_cell(1), do: "\u2581"
  def render_cell(2), do: "\u2582"
  def render_cell(3), do: "\u2583"
  def render_cell(4), do: "\u2584"
  def render_cell(5), do: "\u2585"
  def render_cell(6), do: "\u2586"
  def render_cell(7), do: "\u2587"
  def render_cell(8), do: "\u2588"
  def render_cell(9), do: "\u2592"

  def render(map) do
    for i <- 1..@h do
      for j <- 1..@w, cell = Map.get(map, {i, j}), into: "" do
        render_cell(cell)
      end
    end
    |> Enum.join("\n")
  end

  def p1 do
    start = input()

    {_, flashes} =
      for i <- 1..100, reduce: {start, 0} do
        {curr, flashes} ->
          IO.puts(IO.ANSI.clear())
          IO.puts("p1: step #{i}, flashed: #{flashes}")
          IO.puts(render(curr))
          Process.sleep(50)
          {next, new_flashes} = tick(curr)
          {next, flashes + new_flashes}
      end

    flashes
  end

  def p2 do
    start = input()

    Stream.unfold({start, 1}, fn {curr, step} ->
      IO.puts(IO.ANSI.clear())
      IO.puts("p2: step #{step}")
      IO.puts(render(curr))
      Process.sleep(10)

      {next, flashes} = tick(curr)

      if flashes == 100, do: nil, else: {{curr, step}, {next, step + 1}}
    end)
    |> Stream.run()
  end
end
