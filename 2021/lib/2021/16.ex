import AOC

aoc 2021, 16 do
  def to_binary_digits(?0), do: [0, 0, 0, 0]
  def to_binary_digits(?1), do: [0, 0, 0, 1]
  def to_binary_digits(?2), do: [0, 0, 1, 0]
  def to_binary_digits(?3), do: [0, 0, 1, 1]
  def to_binary_digits(n) when n in ?4..?7, do: [0 | Integer.digits(n - ?0, 2)]
  def to_binary_digits(n) when n in ?8..?9, do: Integer.digits(n - ?0, 2)
  def to_binary_digits(n) when n in ?A..?F, do: Integer.digits(n - ?A + 10, 2)

  def input_bitstring(input_string \\ input_string()) do
    input_string
    |> String.trim()
    |> String.to_charlist()
    |> Enum.flat_map(&to_binary_digits/1)
    |> Enum.into(<<>>, &<<&1::1>>)
  end

  def decode_packets(bits) do
    case decode_packet(bits) do
      {:ok, packet, rest} -> [packet | decode_packets(rest)]
      _ -> []
    end
  end

  def decode_packets(bits, 0) do
    {[], bits}
  end

  def decode_packets(bits, n) do
    {:ok, packet, rest} = decode_packet(bits)
    {packets, rest} = decode_packets(rest, n - 1)
    {[packet | packets], rest}
  end

  def decode_packet(<<version::3, 4::3, rest::bits>>) do
    {value, rest} = decode_value(rest)

    {:ok, [version, 4, Integer.undigits(value, 16)], rest}
  end

  def decode_packet(<<version::3, id::3, 0::1, subpacket_length::15, rest::bits>>) do
    <<subpackets::size(subpacket_length), rest::bits>> = rest

    {:ok, [version, id, decode_packets(<<subpackets::size(subpacket_length)>>)], rest}
  end

  def decode_packet(<<version::3, id::3, 1::1, number_of_subpackets::11, rest::bits>>) do
    {packets, rest} = decode_packets(<<rest::bits>>, number_of_subpackets)

    {:ok, [version, id, packets], rest}
  end

  def decode_packet(_), do: :error

  def decode_value(<<0::1, chunk::4, rest::bits>>), do: {[chunk], rest}

  def decode_value(<<1::1, chunk::4, rest::bits>>) do
    {bits, rest} = decode_value(rest)
    {[chunk | bits], rest}
  end

  def versions([version, _, subpackets]) when is_list(subpackets) do
    [version | Enum.map(subpackets, &versions/1)]
  end
  def versions([version, _, _]), do: [version]


  def eval([_, 4, v]), do: v
  def eval([_, op, args]) do
    case {op, Enum.map(args, &eval/1)} do
      {0 = _sum, args} -> Enum.sum(args)
      {1 = _prod, args} -> Enum.product(args)
      {2 = _min, args} -> Enum.min(args)
      {3 = _max, args} -> Enum.max(args)
      {5 = _gt, [a, b]} -> if a > b, do: 1, else: 0
      {6 = _lt, [a, b]} -> if a < b, do: 1, else: 0
      {7 = _eq, [a, b]} -> if a == b, do: 1, else: 0
    end
  end

  def p1 do
    {:ok, packet, _rest} =
      input_bitstring()
      |> decode_packet()

    versions(packet)
    |> List.flatten()
    |> Enum.sum()
  end

  def p2 do
    {:ok, packet, _rest} =
      input_bitstring()
      |> decode_packet()


    eval(packet)
  end
end
