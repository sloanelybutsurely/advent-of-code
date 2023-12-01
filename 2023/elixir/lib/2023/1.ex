import AOC

aoc 2023, 1 do
  def p1(input) do
    lines = String.split(input, "\n")

    firsts =
      lines
      |> Stream.map(&simple_to_digits/1)
      |> Stream.map(&List.first/1)

    lasts =
      lines
      |> Stream.map(&String.reverse/1)
      |> Stream.map(&simple_to_digits/1)
      |> Stream.map(&List.first/1)

    Stream.zip_with(firsts, lasts, &digits_to_number/2)
    |> Enum.sum()
  end

  def p2(input) do
    lines = String.split(input, "\n")

    firsts =
      lines
      |> Stream.map(&to_digits/1)
      |> Stream.map(&List.first/1)

    lasts =
      lines
      |> Stream.map(&String.reverse/1)
      |> Stream.map(&to_digits_reversed/1)
      |> Stream.map(&List.first/1)

    Stream.zip_with(firsts, lasts, &digits_to_number/2)
    |> Enum.sum()
  end

  def digits_to_number(f, l), do: String.to_integer("#{f}#{l}")

  numbers = [
    {"1", "one", 1},
    {"2", "two", 2},
    {"3", "three", 3},
    {"4", "four", 4},
    {"5", "five", 5},
    {"6", "six", 6},
    {"7", "seven", 7},
    {"8", "eight", 8},
    {"9", "nine", 9}
  ]

  def simple_to_digits(""), do: []

  for {n, _, v} <- numbers do
    def simple_to_digits(unquote(n) <> rest), do: [unquote(v) | simple_to_digits(rest)]
  end

  def simple_to_digits(<<_::binary-size(1), rest::binary>>), do: simple_to_digits(rest)

  def to_digits(""), do: []

  for {n, w, v} <- numbers do
    def to_digits(unquote(n) <> rest), do: [unquote(v) | to_digits(rest)]
    def to_digits(unquote(w) <> rest), do: [unquote(v) | to_digits(rest)]
  end

  def to_digits(<<_::binary-size(1), rest::binary>>), do: to_digits(rest)

  def to_digits_reversed(""), do: []

  for {n, w, v} <- numbers do
    wr = String.reverse(w)
    def to_digits_reversed(unquote(n) <> rest), do: [unquote(v) | to_digits_reversed(rest)]
    def to_digits_reversed(unquote(wr) <> rest), do: [unquote(v) | to_digits_reversed(rest)]
  end

  def to_digits_reversed(<<_::binary-size(1), rest::binary>>), do: to_digits_reversed(rest)
end
