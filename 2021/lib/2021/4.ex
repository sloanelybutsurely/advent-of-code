import AOC

defmodule BingoPlayer do
  use GenServer

  defstruct [:board, :uncalled_numbers, :positions]

  @impl GenServer
  def init(board) do
    numbers =
      board
      |> List.flatten()
      |> MapSet.new()

    {board, positions} =
      for {row, x} <- Enum.with_index(board), {cell, y} <- Enum.with_index(row) do
        {{{x, y}, {cell, false}}, {cell, {x, y}}}
      end
      |> Enum.unzip()

    board = Map.new(board)
    positions = Map.new(positions)

    state = %__MODULE__{
      board: board,
      uncalled_numbers: numbers,
      positions: positions
    }

    {:ok, state}
  end

  def call_number(pid, n) do
    GenServer.call(pid, {:number_called, n})
  end

  @impl GenServer
  def handle_call(
        {:number_called, n},
        _from,
        %__MODULE__{board: board, uncalled_numbers: uncalled_numbers, positions: positions} =
          state
      ) do
    if MapSet.member?(uncalled_numbers, n) do
      uncalled_numbers = MapSet.delete(uncalled_numbers, n)
      board = Map.put(board, Map.get(positions, n), {n, true})

      state = %{state | uncalled_numbers: uncalled_numbers, board: board}

      if is_winning_board?(board) do
        # Prevent winning boards from continuing to play
        uncalled_numbers = MapSet.new()
        state = %{state | uncalled_numbers: uncalled_numbers}
        {:reply, {:win, score_winning_board(board, n)}, state}
      else
        {:reply, :hit, state}
      end
    else
      {:reply, :noop, state}
    end
  end

  defp is_winning_board?(board) do
    Enum.any?(0..4, fn x ->
      Enum.all?(0..4, fn y ->
        Map.get(board, {x, y}) |> then(&elem(&1, 1))
      end)
    end) or
      Enum.any?(0..4, fn y ->
        Enum.all?(0..4, fn x ->
          Map.get(board, {x, y}) |> then(&elem(&1, 1))
        end)
      end)
  end

  defp score_winning_board(board, last_called) do
    sum =
      board
      |> Map.values()
      |> Enum.reject(&elem(&1, 1))
      |> Enum.map(&elem(&1, 0))
      |> Enum.sum()

    sum * last_called
  end
end

aoc 2021, 4 do
  def parse_input() do
    [calls | boards] = input_string() |> String.split("\n\n", trim: true)

    calls = String.split(calls, ",", trim: true) |> Enum.map(&String.to_integer/1)

    boards =
      for board <- boards do
        for row <- String.split(board, "\n") do
          String.split(row, ~r/\W+/, trim: true)
          |> Enum.map(&String.to_integer/1)
        end
      end

    {calls, boards}
  end

  def p1 do
    {calls, boards} = parse_input()

    players =
      for board <- boards do
        {:ok, pid} = GenServer.start_link(BingoPlayer, board)
        pid
      end

    {:halt, score} =
      for call <- calls, player <- players, reduce: :cont do
        {:halt, _} = resp ->
          resp

        :cont ->
          case BingoPlayer.call_number(player, call) do
            {:win, score} -> {:halt, score}
            _ -> :cont
          end
      end

    Enum.each(players, &GenServer.stop/1)

    score
  end

  def p2 do
    {calls, boards} = parse_input()

    players =
      for board <- boards do
        {:ok, pid} = GenServer.start_link(BingoPlayer, board)
        pid
      end

    [last_winning_score | _scores] =
      for call <- calls, player <- players, reduce: [] do
        acc ->
          case BingoPlayer.call_number(player, call) do
            {:win, 0} -> acc
            {:win, score} -> [score | acc]
            _ -> acc
          end
      end

    Enum.each(players, &GenServer.stop/1)

    last_winning_score
  end
end
