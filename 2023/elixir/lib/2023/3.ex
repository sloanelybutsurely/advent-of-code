import AOC

aoc 2023, 3 do
  def p1(input) do
    grid =
      input
      |> String.split("\n")
      |> Enum.map(&String.to_charlist/1)
      |> to_grid()

    {_, part_numbers} =
      for {coord, symb} <- grid,
          is_binary(symb),
          neighbor_coord <- neighboring_coords(coord),
          reduce: {MapSet.new(), []} do
        {seen, part_numbers} ->
          case Map.get(grid, neighbor_coord) do
            {id, n} ->
              if MapSet.member?(seen, id) do
                {seen, part_numbers}
              else
                {MapSet.put(seen, id), [n | part_numbers]}
              end

            _ ->
              {seen, part_numbers}
          end
      end

    part_numbers
    |> Enum.sum()
  end

  def p2(_input) do
  end

  def to_grid(lines) do
    {grid, [], nil, _} =
      for {line, i} <- Enum.with_index(lines),
          {char, j} <- Enum.with_index(line),
          reduce: {Map.new(), [], nil, 0} do
        {acc, digits, l, id} ->
          case {char, digits, l} do
            {c, digits, curr} when c in ?0..?9 and curr in [i, nil] ->
              {acc, [{{i, j}, c} | digits], i, id}

            {c, digits, _} when c in ?0..?9 ->
              {insert_number_into_grid(acc, digits, id), [{{i, j}, c}], i, id + 1}

            {?., [], _} ->
              {acc, [], nil, id}

            {?., digits, _} ->
              {insert_number_into_grid(acc, digits, id), [], nil, id + 1}

            {c, [], _} ->
              {Map.put(acc, {i, j}, List.to_string([c])), [], nil, id}

            {c, digits, _} ->
              {
                acc
                |> Map.put({i, j}, List.to_string([c]))
                |> insert_number_into_grid(digits, id),
                [],
                nil,
                id + 1
              }
          end
      end

    grid
  end

  def insert_number_into_grid(grid, coords_and_digits, id) do
    {coords, rev_digits} = Enum.unzip(coords_and_digits)

    number =
      rev_digits
      |> Enum.reverse()
      |> List.to_string()
      |> String.to_integer()

    Stream.zip(coords, Stream.repeatedly(fn -> {id, number} end))
    |> Enum.into(%{})
    |> Map.merge(grid)
  end

  def neighboring_coords({i, j}) do
    for y <- [i - 1, i, i + 1], x <- [j - 1, j, j + 1], {y, x} != {i, j}, y >= 0, x >= 0 do
      {y, x}
    end
  end
end
