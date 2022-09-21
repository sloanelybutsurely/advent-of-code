import AOC

aoc 2015, 18 do
  def input() do
    lists =
      input_string()
      |> String.split("\n", trim: true)
      |> Enum.map(fn line ->
        line
        |> String.split("", trim: true)
        |> Enum.map(&decode_state/1)
        |> Enum.with_index(1)
      end)
      |> Enum.with_index(1)

    for {row, i} <- lists, {cell, j} <- row, into: %{} do
      {{i, j}, cell}
    end
  end

  def decode_state("#"), do: :on
  def decode_state("."), do: :off

  def encode_state(:on), do: "#"
  def encode_state(:off), do: :off

  def n({i, j}), do: {i - 1, j}
  def e({i, j}), do: {i, j + 1}
  def s({i, j}), do: {i + 1, j}
  def w({i, j}), do: {i, j - 1}
  def ne(pos), do: pos |> n() |> e()
  def se(pos), do: pos |> s() |> e()
  def sw(pos), do: pos |> s() |> w()
  def nw(pos), do: pos |> n() |> w()

  def neighbors(map, pos) do
    [&n/1, &ne/1, &e/1, &se/1, &s/1, &sw/1, &w/1, &nw/1]
    |> Enum.map(&apply(&1, [pos]))
    |> Enum.map(&Map.get(map, &1, :off))
    |> Enum.frequencies()
  end

  def tick(map) do
    for {pos, cell} <- map, into: %{} do
      next_cell =
        case {cell, neighbors(map, pos)} do
          {:on, %{on: on}} when on in [2, 3] -> :on
          {:on, _} -> :off
          {:off, %{on: 3}} -> :on
          {:off, _} -> :off
        end

      {pos, next_cell}
    end
  end

  def tick_with_corners_on(map) do
    for {pos, cell} <- map, into: %{} do
      if pos in [{1, 1}, {1, 100}, {100, 1}, {100, 100}] do
        {pos, :on}
      else
        next_cell =
          case {cell, neighbors(map, pos)} do
            {:on, %{on: on}} when on in [2, 3] -> :on
            {:on, _} -> :off
            {:off, %{on: 3}} -> :on
            {:off, _} -> :off
          end

        {pos, next_cell}
      end
    end
  end

  def p1 do
    start = input()

    finish =
      for _ <- 1..100, reduce: start do
        curr -> tick(curr)
      end

    finish
    |> Map.values()
    |> Enum.count(&(&1 == :on))
  end

  def p2 do
    start =
      input()
      |> Map.put({1, 1}, :on)
      |> Map.put({1, 100}, :on)
      |> Map.put({100, 1}, :on)
      |> Map.put({100, 100}, :on)

    finish =
      for _ <- 1..100, reduce: start do
        curr -> tick_with_corners_on(curr)
      end

    finish
    |> Map.values()
    |> Enum.count(&(&1 == :on))
  end
end
