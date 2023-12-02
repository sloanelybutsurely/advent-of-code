import AOC

aoc 2023, 2 do
  import NimbleParsec

  dice =
    integer(min: 1)
    |> ignore(string(" "))
    |> choice([
      string("blue"),
      string("green"),
      string("red")
    ])
    |> label("die")
    |> optional(ignore(string(", ")))
    |> times(min: 1, max: 3)
    |> label("dice")

  game =
    ignore(string("Game "))
    |> integer(min: 1)
    |> ignore(string(": "))
    |> label("game")

  defparsec(:game_p, game)
  defparsec(:dice_p, dice)

  def read_game(input) do
    case game_p(input) do
      {:ok, [id], rest, _, _, _} ->
        dice_and_rest =
          Stream.unfold(rest, fn
            nil ->
              nil

            rest ->
              case dice_p(rest) do
                {:ok, dice, rest, _, _, _} ->
                  dice =
                    dice
                    |> Enum.chunk_every(2)
                    |> Enum.map(&Enum.reverse/1)
                    |> Enum.map(&List.to_tuple/1)
                    |> Enum.into(%{})

                  {dice, String.replace_prefix(rest, "; ", "")}

                {:error, _, rest, _, _, _} ->
                  {rest, nil}
              end
          end)
          |> Enum.to_list()

        [rest | dice] = Enum.reverse(dice_and_rest)

        {:ok, {id, dice, rest}}

      _ ->
        :halt
    end
  end

  def read_games(input) do
    Stream.unfold(input, fn rest ->
      input =
        rest
        |> String.replace_prefix("\n", "")

      case read_game(input) do
        {:ok, {id, dice, rest}} ->
          {{id, dice}, rest}

        _ ->
          nil
      end
    end)
  end

  def p1(input) do
    red_max = 12
    green_max = 13
    blue_max = 14

    input
    |> read_games()
    |> Stream.reject(fn {_id, dice} ->
      dice
      |> Enum.any?(fn d ->
        Map.get(d, "red", 0) > red_max or Map.get(d, "green", 0) > green_max or
          Map.get(d, "blue", 0) > blue_max
      end)
    end)
    |> Stream.map(&elem(&1, 0))
    |> Enum.sum()
  end

  def p2(_input) do
  end
end
