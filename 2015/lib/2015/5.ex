import AOC

aoc 2015, 5 do
  @deny_list ~w(ab cd pq xy)

  def p1 do
    input_stream()
    |> Stream.filter(&is_string_nice_p1?/1)
    |> Enum.count()
  end

  defp is_string_nice_p1?(str) do
    contains_at_least_three_vowels = String.match?(str, ~r/(?:[aeiou].*){3}/)
    contains_double_letter = String.match?(str, ~r/(\w)\1/)
    does_not_contain_denylist = not Enum.any?(@deny_list, &String.contains?(str, &1))

    contains_at_least_three_vowels and contains_double_letter and does_not_contain_denylist
  end

  def p2 do
    input_stream()
    |> Stream.filter(&is_string_nice_p2?/1)
    |> Enum.count()
  end

  defp is_string_nice_p2?(str) do
    String.match?(str, ~r/(\w)(\w).*\1\2/) and String.match?(str, ~r/(\w)\w\1/)
  end
end
