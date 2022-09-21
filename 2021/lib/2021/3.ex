import AOC

aoc 2021, 3 do
  def p1 do
    [first] = Stream.take(input_stream(), 1) |> Enum.to_list()

    bit_length = String.length(first)

    tallies = :atomics.new(bit_length, [])

    Stream.concat([first], input_stream())
    |> Stream.each(fn value ->
      for {v, i} <- Enum.with_index(String.split(value, "", trim: true), 1) do
        :atomics.add(tallies, i, String.to_integer(v))
      end
    end)
    |> Stream.run()

    gamma_str =
      for b <- 1..bit_length, into: "" do
        if :atomics.get(tallies, b) > 500 do
          "1"
        else
          "0"
        end
      end

    epsilon_str =
      for b <- String.split(gamma_str, "", trim: true), into: "" do
        if b == "1", do: "0", else: "1"
      end

    gamma = String.to_integer(gamma_str, 2)
    epsilon = String.to_integer(epsilon_str, 2)
    gamma * epsilon
  end

  def p2 do
    input_list =
      input_stream()
      |> Enum.map(fn word ->
        String.split(word, "", trim: true) |> Enum.map(&String.to_integer/1)
      end)

    word_size = input_list |> hd() |> length()

    [o2_gen_rating] =
      Enum.reduce(0..word_size, input_list, fn
        _bit, [value] ->
          [value]

        bit_index, list ->
          total_values = length(list)

          bit_sum =
            for word <- list, reduce: 0 do
              acc -> acc + Enum.at(word, bit_index)
            end

          matching_bit = if bit_sum >= total_values / 2, do: 1, else: 0

          Enum.filter(list, fn word -> Enum.at(word, bit_index) == matching_bit end)
      end)

    o2_gen_rating = Enum.join(o2_gen_rating) |> String.to_integer(2)

    [co2_scrub_rating] =
      Enum.reduce(0..word_size, input_list, fn
        _bit, [value] ->
          [value]

        bit_index, list ->
          total_values = length(list)

          bit_sum =
            for word <- list, reduce: 0 do
              acc -> acc + Enum.at(word, bit_index)
            end

          matching_bit = if bit_sum >= total_values / 2, do: 0, else: 1

          Enum.filter(list, fn word -> Enum.at(word, bit_index) == matching_bit end)
      end)

    co2_scrub_rating = Enum.join(co2_scrub_rating) |> String.to_integer(2)

    o2_gen_rating * co2_scrub_rating
  end
end
