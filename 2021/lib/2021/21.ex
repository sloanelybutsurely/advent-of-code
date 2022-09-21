import AOC


aoc 2021, 21 do

  def p1 do
    {total_rolls, {{_, losing_score}, _}} = input_players()
    |> play_game(deterministic_die())

    total_rolls * losing_score
  end

  def p2 do
    input_players()
    |> run_dirac_dice_game()
    |> Tuple.to_list()
    |> Enum.max()
  end

  @dirac_dice_winning_score 21
  @quantum_rolls %{ 3 => 1, 4 => 3, 5 => 6, 6 => 7, 7 => 6, 8 => 3, 9 => 1 }

  def run_dirac_dice_game(players, up_to_play \\ :player_1)
  def run_dirac_dice_game({{_, player_1_score}, _}, :player_2) when player_1_score >= @dirac_dice_winning_score, do: {1, 0}
  def run_dirac_dice_game({_, {_, player_2_score}}, :player_1) when player_2_score >= @dirac_dice_winning_score, do: {0, 1}

  def run_dirac_dice_game({player_1, player_2}, :player_1) do
    for {player_1, freq} <- quantum_move(player_1) do
      run_dirac_dice_game({player_1, player_2}, :player_2)
      |> multiply_by(freq)
      |> compact()
    end
    |> compact()
  end

  def run_dirac_dice_game({player_1, player_2}, :player_2) do
    for {player_2, freq} <- quantum_move(player_2) do
      run_dirac_dice_game({player_1, player_2}, :player_1)
      |> multiply_by(freq)
      |> compact()
    end
    |> compact()
  end

  def quantum_move(player) do
    for {roll, freq} <- @quantum_rolls do
      {move(player, roll), freq}
    end
  end

  def multiply_by(xs, c) when is_list(xs), do: Enum.map(xs, &multiply_by(&1, c))
  def multiply_by({a, b}, c), do: {a * c, b * c}

  def compact(xs) when is_list(xs) do
    {as, bs} = Enum.unzip(xs)
    {Enum.sum(as), Enum.sum(bs)}
  end
  def compact(tp) when is_tuple(tp), do: tp

  def play_game(players, die, winning_score \\ 1000) do
    Enum.reduce_while(rolls(die), {0, players}, fn
      roll, {roll_count, {player, other_player}} ->
        {_, score} = player = move(player, roll)

        action = if score >= winning_score, do: :halt, else: :cont

        {action, {roll_count + 3, {other_player, player}}}
    end)
  end

  def move(player, rolls) when is_list(rolls), do: move(player, Enum.sum(rolls))
  def move({position, score}, roll) do
    position = to_board_position(position + roll)
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
