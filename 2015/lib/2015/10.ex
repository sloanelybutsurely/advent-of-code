import AOC

aoc 2015, 10 do
  def look_and_say(s) when is_binary(s),
    do:
      String.to_integer(s)
      |> Integer.digits()
      |> look_and_say()
      |> IO.iodata_to_binary()

  def look_and_say([x | xs]), do: look_and_say(xs, {x, 1})
  def look_and_say([x | xs], {x, c}), do: look_and_say(xs, {x, c + 1})
  def look_and_say([x | xs], state), do: [look_and_say([], state), look_and_say(xs, {x, 1})]
  def look_and_say([], {x, c}), do: [Integer.to_string(c), Integer.to_string(x)]

  def p1 do
    Enum.reduce(1..40, String.trim(input_string()), fn iteration, acc ->
      IO.puts("#{iteration / 40 * 100}%")

      look_and_say(acc)
    end)
    |> String.length()
  end

  def p2 do
    Enum.reduce(1..50, String.trim(input_string()), fn iteration, acc ->
      IO.puts("#{iteration / 50 * 100}%")

      look_and_say(acc)
    end)
    |> String.length()
  end
end
