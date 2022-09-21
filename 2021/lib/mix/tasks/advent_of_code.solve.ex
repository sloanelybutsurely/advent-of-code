defmodule Mix.Tasks.AdventOfCode.Solve do
  use Mix.Task
  import Mix.AdventOfCode

  @shortdoc "Runs solution code with problem input"

  @moduledoc """
  #{@shortdoc}.

  ## Options

  `--input`
    - name of a `.input` file in `priv/inputs/` without the `.input` extension or `-` to read from stdin

  ## Examples

      # Run Day 2, Part 1 solution program against the `2.1.input` file
      $ mix advent_of_code.solve 2.1

      # Run Day 1, Part 2 with a special input
      $ mix advent_of_code.solve 1.2 --input day-1-part-2-small # priv/inputs/day-1-part-2-small.input

      # Run Day 17, Part 1 with input from stdin
  #   $ mix advent_of_code.solve 17.1 --input -
  """

  @switches [input: :string]

  @impl Mix.Task
  def run(args) do
    case OptionParser.parse!(args, strict: @switches) do
      {opts, [selector]} ->
        case stream_for_input(selector, opts) do
          {:indeterminate, stream} ->
            ProgressBar.render_spinner(
              [text: "Processing indeterminate data...", done: "Done."],
              fn -> solve(selector, stream) end
            )
            |> IO.puts()

          {size, stream} ->
            stream =
              stream
              |> Stream.with_index(1)
              |> Stream.each(fn {_, i} -> ProgressBar.render(i, size) end)
              |> Stream.map(fn {l, _} -> l end)

            solve(selector, stream)
            |> IO.puts()
        end

      _ ->
        nil
    end
  end

  defp solve(selector, stream) do
    selector
    |> AdventOfCode.solver()
    |> AdventOfCode.PuzzleSolver.solve(stream)
  end

  defp stream_for_input(selector, opts) do
    case Keyword.fetch(opts, :input) do
      {:ok, "-"} ->
        {:indeterminate, IO.stream()}

      {:ok, name} ->
        Path.join("priv/inputs", "#{name}.input")
        |> get_size_and_stream()

      :error ->
        [day | _] = String.split(selector, ".")

        file = day_input_path(day)

        unless File.exists?(file) or
                 not Mix.shell().yes?("Input missing. Fetch input for day #{day}?") do
          Mix.Task.run("advent_of_code.fetch_input", [day])
        end

        day_input_path(day)
        |> get_size_and_stream()
    end
  end

  def get_size_and_stream(file) do
    stream = File.stream!(file)
    {Enum.count(stream), stream}
  end
end
