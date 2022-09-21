import AOC

aoc 2021, 10 do
  @openers '([{<'
  @closers ')]}>'

  def points_for_invalid_char(?)), do: 3
  def points_for_invalid_char(?]), do: 57
  def points_for_invalid_char(?}), do: 1197
  def points_for_invalid_char(?>), do: 25137

  def points_for_missing_char(?)), do: 1
  def points_for_missing_char(?]), do: 2
  def points_for_missing_char(?}), do: 3
  def points_for_missing_char(?>), do: 4

  def pair_for(?(), do: ?)
  def pair_for(?[), do: ?]
  def pair_for(?{), do: ?}
  def pair_for(?<), do: ?>

  def find_line_errors(inp, state \\ [], inv \\ [])

  def find_line_errors([], state, inv), do: {inv, state}

  def find_line_errors([x | xs], [x | ys], inv), do: find_line_errors(xs, ys, inv)

  def find_line_errors([x | xs], state, inv) when x in @closers,
    do: find_line_errors(xs, Enum.drop(state, 1), [x | inv])

  def find_line_errors([x | xs], ys, inv) when x in @openers,
    do: find_line_errors(xs, [pair_for(x) | ys], inv)

  def score_missing_charlist(charlist, score \\ 0)
  def score_missing_charlist([], score), do: score

  def score_missing_charlist([x | xs], score),
    do: score_missing_charlist(xs, points_for_missing_char(x) + score * 5)

  def input_charlist_stream(), do: input_stream() |> Stream.map(&String.to_charlist/1)

  def invalid_chars_stream(),
    do:
      input_charlist_stream()
      |> Stream.map(&find_line_errors/1)
      |> Stream.reject(&match?({[], _}, &1))
      |> Stream.map(&elem(&1, 0))
      |> Stream.map(&hd/1)

  def missing_charlists_stream(),
    do:
      input_charlist_stream()
      |> Stream.map(&find_line_errors/1)
      |> Stream.filter(&match?({[], _}, &1))
      |> Stream.reject(&match?({_, []}, &1))
      |> Stream.map(&elem(&1, 1))

  def p1 do
    invalid_chars_stream()
    |> Stream.map(&points_for_invalid_char/1)
    |> Enum.sum()
  end

  def p2 do
    scores =
      missing_charlists_stream()
      |> Stream.map(&score_missing_charlist/1)
      |> Enum.sort()

    scores_count = length(scores)

    winner = scores |> Enum.at(div(scores_count, 2))

    winner
  end
end
