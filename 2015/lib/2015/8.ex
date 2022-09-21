import AOC

aoc 2015, 8 do
  def decoded_difference(str, diff \\ 0)
  def decoded_difference("", diff), do: diff
  def decoded_difference(~S[\"] <> rest, diff), do: decoded_difference(rest, diff + 1)
  def decoded_difference(~S[\\] <> rest, diff), do: decoded_difference(rest, diff + 1)
  def decoded_difference(~S["] <> rest, diff), do: decoded_difference(rest, diff + 1)

  def decoded_difference(<<(~S[\x]), _::binary-size(2), rest::binary>>, diff),
    do: decoded_difference(rest, diff + 3)

  def decoded_difference(<<_::binary-size(1), rest::binary>>, diff),
    do: decoded_difference(rest, diff)

  def encoded_difference(str, diff \\ 0)
  def encoded_difference("", diff), do: diff + 2
  def encoded_difference(~S["] <> rest, diff), do: encoded_difference(rest, diff + 1)
  def encoded_difference(~s[\\] <> rest, diff), do: encoded_difference(rest, diff + 1)

  def encoded_difference(<<_::binary-size(1), rest::binary>>, diff),
    do: encoded_difference(rest, diff)

  def p1 do
    input_stream()
    |> Stream.map(&decoded_difference/1)
    |> Enum.sum()
  end

  def p2 do
    input_stream()
    |> Stream.map(&encoded_difference/1)
    |> Enum.sum()
  end
end
