defmodule Mix.Tasks.AdventOfCode.FetchInput do
  use Mix.Task

  import Mix.Generator
  import Mix.AdventOfCode

  @shortdoc "Fetches the inputs for day and saves them in `priv/inputs`"

  @moduledoc """
  #{@shortdoc}.

  ## Examples

      # Fetch day 1's inputs
      $ mix advent_of_code.fetch_input 1
  """

  @finch Mix.AdventOfCode.FetchInput.Finch

  @switches [event: :string, session: :string]

  @impl Mix.Task
  def run(args) do
    Mix.Task.run("app.config")
    :ok = Application.ensure_started(:telemetry)
    {:ok, _pid} = Finch.start_link(name: @finch)

    default_event = Application.get_env(:advent_of_code, :event)
    default_session = Application.get_env(:advent_of_code, :session)

    case OptionParser.parse!(args, strict: @switches) do
      {opts, [day]} ->
        input_file_path = day_input_path(day)

        if overwrite?(input_file_path) do
          event = Keyword.get(opts, :event, default_event)
          session = Keyword.get(opts, :session, default_session)

          {:ok, %{body: data}} =
            Finch.build(
              :get,
              "https://adventofcode.com/#{event}/day/#{day}/input",
              [{"Cookie", "session=#{session}"}]
            )
            |> Finch.request(@finch)

          create_file(input_file_path, data, force: true)
        end

      _ ->
        Mix.raise("Unexpected arguments.")
    end
  end
end
