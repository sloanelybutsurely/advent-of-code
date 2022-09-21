defmodule Mix.Tasks.AdventOfCode.Gen.Solution do
  use Mix.Task
  import Mix.Generator
  import Mix.AdventOfCode

  @shortdoc "Generate a new solution module"

  @moduledoc """
  #{@shortdoc}.

  Includes new solution module, test, and empty problem input.

  ## Examples

      # Generate solution modules, tests, and empty input for Day 2
      $ mix advent_of_code.gen.solution 2
  """

  @switches []

  @impl Mix.Task
  def run(args) do
    case OptionParser.parse!(args, switches: @switches) do
      {_, [day]} ->
        day_module = day_module(day)
        day_contents = day_template(mod: day_module, day: day) |> Code.format_string!()
        day_path = Path.join("lib", Macro.underscore(day_module))
        day_tests_path = Path.join("test", Macro.underscore(day_module))
        day_file = "#{day_path}.ex"

        create_file(day_file, day_contents)

        create_directory(day_tests_path)

        for part <- 1..2 do
          part = to_string(part)
          part_module = part_module(day, part)

          part_test_module = Module.concat(day_module, Macro.camelize("Part#{part}Test"))

          part_test_contents =
            part_test_template(mod: part_module, test_mod: part_test_module)
            |> Code.format_string!()

          part_test_file =
            Path.join(day_tests_path, "#{Macro.underscore("part_#{part}_test")}.exs")

          create_file(part_test_file, part_test_contents)
        end

        day_regex = ~r/\W(#{day})\W/
        readme_file = "README.md"
        readme_contents = File.read!("README.md")

        readme_contents =
          "#{String.replace(readme_contents, day_regex, "[\\1]")}\n[#{day}]: ./#{day_file}\n"

        File.write!(readme_file, readme_contents)
        Mix.shell().info([:green, "* updating ", :reset, "README.md"])

      _ ->
        Mix.raise("Unknown arguments.")
    end
  end

  embed_template(:day, ~S[
    defmodule <%= inspect(@mod) %> do
      @moduledoc """
      Day <%= @day %>
      """
    end

    defmodule <%= inspect(@mod) %>.Part1 do
      @moduledoc """
      Day <%= @day %>, Part 1
      """

      alias AdventOfCode.PuzzleSolver
      use PuzzleSolver

      import <%= inspect(@mod) %>, warn: false

      @impl PuzzleSolver
      def solve(_input_stream) do
        :ok |> to_string()
      end
    end

    defmodule <%= inspect(@mod) %>.Part2 do
      @moduledoc """
      Day <%= @day %>, Part 2
      """

      alias AdventOfCode.PuzzleSolver
      use PuzzleSolver

      import <%= inspect(@mod) %>, warn: false

      @impl PuzzleSolver
      def solve(_input_stream) do
        :ok |> to_string()
      end
    end
  ])

  embed_template(:part_test, ~S[
    defmodule <%= inspect(@test_mod) %> do
      use AdventOfCode.PuzzleCase, module: <%= inspect(@mod) %>

      test "returns :ok" do
        input = ~S"""
        input
        """

        assert_solution input, "ok"
      end
    end
  ])
end
