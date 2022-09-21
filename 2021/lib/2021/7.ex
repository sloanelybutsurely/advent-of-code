import AOC

aoc 2021, 7 do
  use AOCHelpers

  def crabs(), do: input_number_list()

  def p1, do: cost_of_most_efficient_position(&Function.identity/1)

  def p2, do: cost_of_most_efficient_position(&(1 / 2 * &1 * (&1 + 1)))

  def total_fuel_cost_for_position(crab_freqs, pos, cost_fun) do
    crab_freqs
    |> Enum.reduce(0, fn {crab_pos, num_crab}, partial_cost ->
      linear_distance = abs(crab_pos - pos)

      partial_cost + num_crab * cost_fun.(linear_distance)
    end)
  end

  def cost_of_most_efficient_position(cost_fun) when is_function(cost_fun) do
    crab_positions = input_number_list()

    max_crab_position = Enum.max(crab_positions)
    crab_frequencies = Enum.frequencies(crab_positions)

    crab_frequencies
    |> cost_of_most_efficient_position(1..max_crab_position, cost_fun)
  end

  def cost_of_most_efficient_position(crab_freqs, range, cost_fun) do
    range
    |> Enum.map(&total_fuel_cost_for_position(crab_freqs, &1, cost_fun))
    |> Enum.min()
    |> trunc()
  end
end
