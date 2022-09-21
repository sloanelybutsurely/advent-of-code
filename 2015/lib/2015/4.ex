import AOC

aoc 2015, 4 do
  def p1 do
    input_string()
    |> String.trim()
    |> mine_advent_coin(5)
  end

  defp mine_advent_coin(key, difficulty, nonce \\ 0)

  defp mine_advent_coin(key, 5, nonce) do
    case :erlang.md5(key <> Integer.to_string(nonce)) do
      <<0::20, _::108>> -> nonce
      _ -> mine_advent_coin(key, 5, nonce + 1)
    end
  end

  defp mine_advent_coin(key, 6, nonce) do
    case :erlang.md5(key <> Integer.to_string(nonce)) do
      <<0::24, _::104>> -> nonce
      _ -> mine_advent_coin(key, 6, nonce + 1)
    end
  end

  def p2 do
    input_string()
    |> String.trim()
    |> mine_advent_coin(6)
  end
end
