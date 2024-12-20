import AOC
import AOC.Prelude

aoc 2024, 15 do
  def p1(input) do
    {grid, moves} = read_input(input)

    grid
    |> apply_moves(moves)
    |> boxes_coords()
    |> Enum.map(&gps/1)
    |> Enum.sum()
  end

  def p2(input) do
    {grid, moves} =
      input
      |> String.replace("#", "##")
      |> String.replace("O", "[]")
      |> String.replace(".", "..")
      |> String.replace("@", "@.")
      |> read_input()

    grid
    |> apply_moves(moves)
    |> boxes_coords()
    |> Enum.map(&gps/1)
    |> Enum.sum()
  end

  ## calculations

  defp boxes_coords(grid) do
    grid
    |> Enum.filter(&match?({_, c} when c in ["O", "["], &1))
    |> Enum.map(&elem(&1, 0))
  end

  defp gps({x, y}), do: 100 * y + x

  ## movement

  defp apply_moves(grid, moves) do
    Enum.reduce(moves, grid, fn direction, grid ->
      # it would be more efficient to keep track of this and move it around but
      # this is much easier to manage
      robot = find_robot(grid)
      {_, grid} = apply_move(grid, robot, direction)
      grid
    end)
  end

  defp apply_move(grid, pos, direction, check_pair \\ true) do
    dest = move(pos, direction)

    case {Map.get(grid, pos), direction, check_pair} do
      {"#", _, _} ->
        {:error, grid}

      # should never happen with the way inputs are constructed
      {nil, _, _} ->
        {:error, grid}

      {".", _, _} ->
        {:ok, grid}

      # up and down movements of two-width boxes must be able to move both
      # chars
      {char, direction, true} when char in ["[", "]"] and direction in [:up, :down] ->
        shift =
          case char do
            "[" -> :right
            "]" -> :left
          end

        with {:ok, grid} <- apply_move(grid, move(pos, shift), direction, false),
             {:ok, grid} <- apply_move(grid, dest, direction) do
          grid =
            grid
            |> Map.put(dest, char)
            |> Map.put(pos, ".")

          {:ok, grid}
        else
          _ -> {:error, grid}
        end

      # left and right movements of two-width boxes are no different from
      # one-width boxes
      {char, _, _} when char in ["@", "O", "[", "]"] ->
        with {:ok, grid} <- apply_move(grid, dest, direction) do
          grid =
            grid
            |> Map.put(dest, char)
            |> Map.put(pos, ".")

          {:ok, grid}
        end
    end
  end

  defp find_robot(grid) do
    {pos, "@"} = Enum.find(grid, &match?({_, "@"}, &1))

    pos
  end

  ## traversal

  defp move({x, y}, :up), do: {x, y - 1}
  defp move({x, y}, :down), do: {x, y + 1}
  defp move({x, y}, :left), do: {x - 1, y}
  defp move({x, y}, :right), do: {x + 1, y}

  ## input

  defp read_input(input) do
    [grid_input, moves_input] =
      String.split(input, "\n\n")

    grid = map_grid(grid_input)

    moves =
      moves_input
      |> lines()
      |> Enum.join()
      |> String.graphemes()
      |> Enum.map(fn
        "^" -> :up
        "v" -> :down
        "<" -> :left
        ">" -> :right
      end)

    {grid, moves}
  end

  ## output

  # defp render(grid) do
  #   {{max_x, _}, _} = Enum.max_by(grid, fn {{x, _}, _} -> x end)
  #   {{_, max_y}, _} = Enum.max_by(grid, fn {{_, y}, _} -> y end)

  #   for y <- 0..max_y do
  #     line =
  #       for x <- 0..max_x, into: "" do
  #         Map.get(grid, {x, y})
  #       end

  #     IO.puts(line)
  #   end

  #   grid
  # end
end
