import AOC

aoc 2023, 1 do
  def p1(input) do
    lines = String.split(input, "\n")

    firsts =
      lines
      |> Stream.map(&to_digits/1)
      |> Stream.map(&List.first/1)

    lasts =
      lines
      |> Stream.map(&String.reverse/1)
      |> Stream.map(&to_digits/1)
      |> Stream.map(&List.first/1)

    Stream.zip_with(firsts, lasts, &digits_to_number/2)
    |> Enum.sum()
  end

  def p2(input) do
    lines = String.split(input, "\n")

    firsts =
      lines
      |> Stream.map(&to_digits_2/1)
      |> Stream.map(&List.first/1)

    lasts =
      lines
      |> Stream.map(&String.reverse/1)
      |> Stream.map(&to_digits_3/1)
      |> Stream.map(&List.first/1)

    Stream.zip_with(firsts, lasts, &digits_to_number/2)
    |> Enum.sum()
  end

  def digits_to_number(f, l), do: String.to_integer("#{f}#{l}")

  def to_digits(""), do: []
  def to_digits("1" <> rest), do: [1 | to_digits(rest)]
  def to_digits("2" <> rest), do: [2 | to_digits(rest)]
  def to_digits("3" <> rest), do: [3 | to_digits(rest)]
  def to_digits("4" <> rest), do: [4 | to_digits(rest)]
  def to_digits("5" <> rest), do: [5 | to_digits(rest)]
  def to_digits("6" <> rest), do: [6 | to_digits(rest)]
  def to_digits("7" <> rest), do: [7 | to_digits(rest)]
  def to_digits("8" <> rest), do: [8 | to_digits(rest)]
  def to_digits("9" <> rest), do: [9 | to_digits(rest)]
  def to_digits(<<_::binary-size(1), rest::binary>>), do: to_digits(rest)

  def to_digits_2(""), do: []
  def to_digits_2("1" <> rest), do: [1 | to_digits_2(rest)]
  def to_digits_2("one" <> rest), do: [1 | to_digits_2(rest)]
  def to_digits_2("2" <> rest), do: [2 | to_digits_2(rest)]
  def to_digits_2("two" <> rest), do: [2 | to_digits_2(rest)]
  def to_digits_2("3" <> rest), do: [3 | to_digits_2(rest)]
  def to_digits_2("three" <> rest), do: [3 | to_digits_2(rest)]
  def to_digits_2("4" <> rest), do: [4 | to_digits_2(rest)]
  def to_digits_2("four" <> rest), do: [4 | to_digits_2(rest)]
  def to_digits_2("5" <> rest), do: [5 | to_digits_2(rest)]
  def to_digits_2("five" <> rest), do: [5 | to_digits_2(rest)]
  def to_digits_2("6" <> rest), do: [6 | to_digits_2(rest)]
  def to_digits_2("six" <> rest), do: [6 | to_digits_2(rest)]
  def to_digits_2("7" <> rest), do: [7 | to_digits_2(rest)]
  def to_digits_2("seven" <> rest), do: [7 | to_digits_2(rest)]
  def to_digits_2("8" <> rest), do: [8 | to_digits_2(rest)]
  def to_digits_2("eight" <> rest), do: [8 | to_digits_2(rest)]
  def to_digits_2("9" <> rest), do: [9 | to_digits_2(rest)]
  def to_digits_2("nine" <> rest), do: [9 | to_digits_2(rest)]

  def to_digits_2(<<_::binary-size(1), rest::binary>>),
    do: to_digits_2(rest)

  def to_digits_3(""), do: []
  def to_digits_3("1" <> rest), do: [1 | to_digits_3(rest)]
  def to_digits_3("eno" <> rest), do: [1 | to_digits_3(rest)]
  def to_digits_3("2" <> rest), do: [2 | to_digits_3(rest)]
  def to_digits_3("owt" <> rest), do: [2 | to_digits_3(rest)]
  def to_digits_3("3" <> rest), do: [3 | to_digits_3(rest)]
  def to_digits_3("eerht" <> rest), do: [3 | to_digits_3(rest)]
  def to_digits_3("4" <> rest), do: [4 | to_digits_3(rest)]
  def to_digits_3("ruof" <> rest), do: [4 | to_digits_3(rest)]
  def to_digits_3("5" <> rest), do: [5 | to_digits_3(rest)]
  def to_digits_3("evif" <> rest), do: [5 | to_digits_3(rest)]
  def to_digits_3("6" <> rest), do: [6 | to_digits_3(rest)]
  def to_digits_3("xis" <> rest), do: [6 | to_digits_3(rest)]
  def to_digits_3("7" <> rest), do: [7 | to_digits_3(rest)]
  def to_digits_3("neves" <> rest), do: [7 | to_digits_3(rest)]
  def to_digits_3("8" <> rest), do: [8 | to_digits_3(rest)]
  def to_digits_3("thgie" <> rest), do: [8 | to_digits_3(rest)]
  def to_digits_3("9" <> rest), do: [9 | to_digits_3(rest)]
  def to_digits_3("enin" <> rest), do: [9 | to_digits_3(rest)]

  def to_digits_3(<<_::binary-size(1), rest::binary>>),
    do: to_digits_3(rest)
end
