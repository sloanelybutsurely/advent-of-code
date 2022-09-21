defmodule AOCHelpers do
  defmacro __using__(_) do
    quote do
      def input_number_list(sep \\ ",") do
        input_string()
        |> String.trim()
        |> String.split(sep)
        |> Enum.map(&String.to_integer/1)
      end

      def iterate(times, start, fun) do
        Enum.reduce(1..times, start, fn _, current -> fun.(current) end)
      end
    end
  end
end
