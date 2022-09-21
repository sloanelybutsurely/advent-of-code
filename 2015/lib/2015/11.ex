import AOC

aoc 2015, 11 do
  def is_valid_password?(password) do
    includes_increasing_straight_of_three(password) and
      does_not_contain_iol(password) and
      includes_two_pairs_of_double_letters(password)
  end

  def includes_increasing_straight_of_three(b) when is_binary(b),
    do:
      String.to_charlist(b)
      |> includes_increasing_straight_of_three()

  def includes_increasing_straight_of_three([a, b, c | _])
      when a == b - 1 and b == c - 1,
      do: true

  def includes_increasing_straight_of_three([_ | rest]),
    do: includes_increasing_straight_of_three(rest)

  def includes_increasing_straight_of_three(_), do: false

  def does_not_contain_iol(password) do
    not String.match?(password, ~r/[iol]/)
  end

  def includes_two_pairs_of_double_letters(password) do
    String.match?(password, ~r/(\w)\1.*(\w)\2/)
  end

  def increment_password(password) when is_binary(password),
    do:
      password
      |> String.to_charlist()
      |> Enum.reverse()
      |> increment_password()
      |> Enum.reverse()
      |> IO.chardata_to_string()

  def increment_password([]), do: [?a]
  def increment_password([?z | rest]), do: [?a | increment_password(rest)]
  def increment_password([c | rest]), do: [c + 1 | rest]

  def compute_next_password(current_password) do
    candidate = increment_password(current_password)

    if is_valid_password?(candidate) do
      candidate
    else
      compute_next_password(candidate)
    end
  end

  def p1 do
    input_string()
    |> String.trim()
    |> compute_next_password()
  end

  def p2 do
  end
end
