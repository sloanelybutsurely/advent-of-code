defmodule Aoc do
  defmacro input do
    day =
      case Regex.run(~r/^Elixir\.Aoc\.Day(\d+)$/, "#{__CALLER__.module}", capture: :all_but_first) do
        [day_str] ->
          String.to_integer(day_str)

        _ ->
          raise "`input/0` must be called from a module of the format `Aoc.Day<n>`. Called in module #{inspect(__CALLER__.module)}"
      end

    quote do
      input(unquote(day))
    end
  end

  def input(day) do
    input_file = :code.priv_dir(:aoc) |> Path.join("inputs/#{day}.txt")

    if File.exists?(input_file) do
      File.read!(input_file)
    else
      input = download_input(day)

      File.write!(input_file, input)

      input
    end
  end

  def lines(str) do
    String.split(str, "\n", trim: true)
  end

  defp download_input(day) do
    Req.request!(
      url: "https://adventofcode.com/2025/day/#{day}/input",
      headers: [cookie: "session=#{session_cookie()}"]
    ).body
  end

  defp session_cookie do
    Application.fetch_env!(:aoc, :session_cookie)
  end
end
