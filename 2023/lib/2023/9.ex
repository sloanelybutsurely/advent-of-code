import AOC
import AOCHelpers

aoc 2023, 9 do
  def p1(input) do
    input
    |> lines_of_integers()
    |> Enum.map(&next_in_sequence/1)
    |> Enum.sum()
  end

  def p2(input) do
    input
    |> lines_of_integers()
    |> Enum.map(&prev_in_sequence/1)
    |> Enum.sum()
  end

  def next_in_sequence(xs) do
    xs
    |> differentiate()
    |> Enum.map(&List.last/1)
    |> Enum.sum()
  end

  def prev_in_sequence(xs) do
    xs
    |> differentiate()
    |> Enum.map(&List.first/1)
    |> Enum.reverse()
    |> Enum.reduce(&(&1 - &2))
  end

  def differentiate(xs) do
    xs
    |> Stream.iterate(&deltas/1)
    |> Enum.take_while(&(not all_zeros?(&1)))
  end

  def deltas(xs), do: Enum.zip_with(xs, Enum.drop(xs, 1), &(&2 - &1))

  def all_zeros?(xs), do: Enum.all?(xs, is?(0))
end
