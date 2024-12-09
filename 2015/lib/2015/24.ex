import AOC

aoc 2015, 24 do
  def p1 do
    packages = read_packages()
    target = packages |> Enum.sum() |> div(3)

    distribute_packages(packages, target, List.duplicate([], 3))
  end

  def p2 do
  end

  ## solution

  def distribute_packages(nums, bin_count) do
    target =
      nums
      |> Enum.sum()
      |> div(bin_count)

    bins = List.duplicate([], bin_count)

    distribute_packages(nums, target, bins)
  end

  def distribute_packages([], target, bins) do
    if Enum.all?(bins, &(Enum.sum(&1) == target)) do
      [bins]
    else
      []
    end
  end

  def distribute_packages([n | rest], target, bins) do
    if Enum.any?(bins, &(Enum.sum(&1) > target)) do
      []
    else
      Enum.flat_map(0..(length(bins) - 1), fn i ->
        updated_bins = List.update_at(bins, i, &[n | &1])
        distribute_packages(rest, target, updated_bins)
      end)
    end
  end

  ## input

  defp read_packages do
    input_string()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end
