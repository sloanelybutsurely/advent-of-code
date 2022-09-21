import AOC

aoc 2015, 7 do
  import NimbleParsec
  use Bitwise

  defparsec(
    :wire_label,
    ascii_string([?a..?z], min: 1)
    |> unwrap_and_tag(:wire)
  )

  defparsec(
    :connects_to,
    ignore(string(" -> "))
  )

  defparsec(
    :signal,
    integer(min: 1)
    |> unwrap_and_tag(:signal)
  )

  defparsec(
    :input,
    choice([
      parsec(:signal),
      parsec(:wire_label)
    ])
  )

  defparsec(
    :and,
    parsec(:input)
    |> ignore(string(" AND "))
    |> parsec(:input)
    |> tag(:and)
  )

  defparsec(
    :or,
    parsec(:input)
    |> ignore(string(" OR "))
    |> parsec(:input)
    |> tag(:or)
  )

  defparsec(
    :lshift,
    parsec(:wire_label)
    |> ignore(string(" LSHIFT "))
    |> integer(min: 1)
    |> tag(:lshift)
  )

  defparsec(
    :rshift,
    parsec(:wire_label)
    |> ignore(string(" RSHIFT "))
    |> integer(min: 1)
    |> tag(:rshift)
  )

  defparsec(
    :not,
    ignore(string("NOT "))
    |> parsec(:wire_label)
    |> tag(:not)
  )

  defparsec(
    :instruction_parsec,
    choice([
      parsec(:and),
      parsec(:or),
      parsec(:lshift),
      parsec(:rshift),
      parsec(:not),
      parsec(:input)
    ])
    |> parsec(:connects_to)
    |> parsec(:wire_label)
  )

  def unwrap_parsec_result({:ok, [{tag, attrs}, {:wire, assign}], _, _, _, _}) do
    attrs = List.wrap(attrs)
    {assign, List.to_tuple([tag | attrs])}
  end

  def parse_instruction(str),
    do:
      str
      |> instruction_parsec()
      |> unwrap_parsec_result()

  def circuit(), do: input_stream() |> Stream.map(&parse_instruction/1) |> Enum.into(%{})

  def p1 do
    :ets.new(:memo, [:set, :named_table])

    circuit()
    |> trace("a")
  end

  def p2 do
    :ets.new(:memo, [:set, :named_table])

    circuit()
    |> Map.put("b", 956)
    |> trace("a")
  end

  def trace(circuit, wire) when is_binary(wire) do
    case :ets.lookup(:memo, wire) do
      [{^wire, value}] ->
        value

      _ ->
        value = trace(circuit, Map.get(circuit, wire))
        :ets.insert(:memo, {wire, value})
        value
    end
  end

  def trace(_circuit, signal) when is_integer(signal) do
    signal
  end

  def trace(circuit, {:wire, wire}) do
    trace(circuit, wire)
  end

  def trace(circuit, {:signal, signal}) do
    trace(circuit, signal)
  end

  def trace(circuit, {:or, lhs, rhs}) do
    trace(circuit, lhs) ||| trace(circuit, rhs)
  end

  def trace(circuit, {:and, lhs, rhs}) do
    trace(circuit, lhs) &&& trace(circuit, rhs)
  end

  def trace(circuit, {:lshift, lhs, rhs}) do
    trace(circuit, lhs) <<< trace(circuit, rhs)
  end

  def trace(circuit, {:rshift, lhs, rhs}) do
    trace(circuit, lhs) >>> trace(circuit, rhs)
  end

  def trace(circuit, {:not, arg}) do
    ~~~trace(circuit, arg)
  end

  def trace(_circuit, other), do: other
end
