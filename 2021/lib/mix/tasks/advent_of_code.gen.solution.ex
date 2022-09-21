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
        day_contents = day_template(mod: day_module) |> Code.format_string!()
        day_path = Path.join("lib", Macro.underscore(day_module))
        day_tests_path = Path.join("test", Macro.underscore(day_module))
        day_file = "#{day_path}.ex"

        create_directory(day_path)
        create_directory(day_tests_path)
        create_file(day_file, day_contents)

        for part <- 1..2 do
          part_module = part_module(day, part)

          part_contents =
            part_template(mod: part_module, day_mod: day_module) |> Code.format_string!()

          part_file = Path.join("lib", "#{Macro.underscore(part_module)}.ex")
          create_file(part_file, part_contents)

          part_test_module = Module.concat(day_module, Macro.camelize("Part#{part}Test"))

          part_test_contents =
            part_test_template(mod: part_module, test_mod: part_test_module)
            |> Code.format_string!()

          part_test_file =
            Path.join(day_tests_path, "#{Macro.underscore("part_#{part}_test")}.exs")

          create_file(part_test_file, part_test_contents)
        end

      _ ->
        Mix.raise("Unknown arguments.")
    end
  end

  embed_template(:day, ~S"""
    defmodule <%= inspect(@mod) %> do

    end
  """)

  embed_template(:part, ~S"""
    defmodule <%= inspect(@mod) %> do
      alias AdventOfCode.PuzzleSolver
      use PuzzleSolver

      import <%= inspect(@day_mod) %>, warn: false

      @impl PuzzleSolver
      def solve(_input_stream) do
        :ok |> to_string()
      end
    end
  """)

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
