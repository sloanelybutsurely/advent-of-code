import AOC


aoc 2021, 21 do

  def p1 do
    {total_rolls, {{_, losing_score}, _}} = input_players()
    |> play_game(deterministic_die())

    total_rolls * losing_score
  end

  def p2 do
  end

  def play_game(players, die, winning_score \\ 1000) do
    Enum.reduce_while(rolls(die), {0, players}, fn
      roll, {roll_count, {player, other_player}} ->
        {_, score} = player = move(player, roll)

        action = if score >= winning_score, do: :halt, else: :cont

        {action, {roll_count + 3, {other_player, player}}}
    end)
  end

  def move({position, score}, roll) do
    position = (position + Enum.sum(roll)) |> to_board_position()
    score = score + position

    {position, score}
  end

  def to_board_position(n) do
    1..10 |> Enum.at(Integer.mod(n, 10) - 1)
  end

  def rolls(die, roll_size \\ 3), do: Stream.chunk_every(die, roll_size)

  def deterministic_die() do
    Stream.iterate(1, fn
      100 -> 1
      n -> n + 1
    end)
  end

  def input_players() do
    input_stream()
    |> Enum.map(fn line ->
      [position] = Regex.run(~r/Player \d starting position: (\d+)/, line, capture: :all_but_first)

      {String.to_integer(position), 0}
    end)
    |> List.to_tuple()
  end
end
