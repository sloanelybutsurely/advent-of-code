import AOC

aoc 2022, 7 do
  def p1(input) do
    {sized_dirs, _} = input
    |> String.split("\n")
    |> Enum.flat_map(&transform_input_line/1)
    |> Enum.reduce({nil, %{}}, &update_state/2)
    |> then(fn {_, fs} -> fs end)
    |> calculate_dir_size("/")

    sized_dirs
    |> Enum.map(&elem(&1, 1))
    |> Enum.reject(& &1 > 100_000)
    |> Enum.sum()
  end

  def p2(_input) do
  end

  defp transform_input_line("$ cd " <> path), do: [{:cd, path}]
  defp transform_input_line("$ ls" <> _), do: []
  defp transform_input_line("dir " <> d), do: [{:d, d}]
  defp transform_input_line(file) do
    [size_s, f] = String.split(file, " ")
    [{:f, f, String.to_integer(size_s)}]
  end

  defp update_state({:cd, path}, {cwd, fs}), do: {cd(cwd, path), fs}
  defp update_state({:d, d}, {cwd, fs}), do: {cwd, Map.update(fs, cwd, [Path.join(cwd, d)], & [Path.join(cwd, d) | &1])}
  defp update_state({:f, _, f}, {cwd, fs}), do: {cwd, Map.update(fs, cwd, [f], & [f | &1])}
  defp update_state(_, s), do: s

  defp cd(_cwd, "/" <> _ = path), do: path
  defp cd(cwd, path), do: Path.join(cwd, path) |> Path.expand()

  defp calculate_dir_size(fs, path) do
    {updated_fs, total} = for item <- fs[path], reduce: {fs, 0} do
      {acc_fs, sum} ->
        case item do
          f when is_integer(f) -> {acc_fs, sum + f}
          d when is_binary(d) -> 
            {acc_fs_, v} = calculate_dir_size(acc_fs, d)
            {acc_fs_, v + sum}
        end
    end
    {Map.put(updated_fs, path, total), total}
  end

end
