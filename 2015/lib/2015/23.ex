import AOC

aoc 2015, 23 do
  defmodule Program do
    import Integer, only: [is_even: 1]

    @enforce_keys [:instructions]
    defstruct [:instructions, ptr: 0, a: 0, b: 0]

    def new(inp) do
      instructions =
        for {line, idx} <- Enum.with_index(inp), into: %{} do
          {idx, parse_line(line)}
        end

      %Program{instructions: instructions}
    end

    def run(%Program{ptr: ptr, instructions: instructions} = prgm) do
      case Map.fetch(instructions, ptr) do
        {:ok, inst} ->
          prgm
          |> exec_inst(inst)
          |> run()

        :error ->
          prgm
      end
    end

    defp exec_inst(prgm, {:hlf, r}) do
      prgm
      |> update!(r, &div(&1, 2))
      |> step()
    end

    defp exec_inst(prgm, {:tpl, r}) do
      prgm
      |> update!(r, &(&1 * 3))
      |> step()
    end

    defp exec_inst(prgm, {:inc, r}) do
      prgm
      |> update!(r, &(&1 + 1))
      |> step()
    end

    defp exec_inst(prgm, {:jmp, offset}) do
      update!(prgm, :ptr, &(&1 + offset))
    end

    defp exec_inst(prgm, {:jie, r, offset}) do
      if prgm |> fetch!(r) |> is_even() do
        update!(prgm, :ptr, &(&1 + offset))
      else
        step(prgm)
      end
    end

    defp exec_inst(prgm, {:jio, r, offset}) do
      if fetch!(prgm, r) == 1 do
        update!(prgm, :ptr, &(&1 + offset))
      else
        step(prgm)
      end
    end

    def reg(%Program{} = prgm, r) do
      prgm
      |> Map.get(reg_key(r))
    end

    defp reg_key("a"), do: :a
    defp reg_key("b"), do: :b
    defp reg_key(other), do: other

    defp update!(%Program{} = prgm, r, fun) do
      Map.update!(prgm, reg_key(r), fun)
    end

    def fetch!(%Program{} = prgm, r) do
      Map.fetch!(prgm, reg_key(r))
    end

    defp step(prgm), do: update!(prgm, :ptr, &(&1 + 1))

    defp parse_line("hlf " <> r), do: {:hlf, r}
    defp parse_line("tpl " <> r), do: {:tpl, r}
    defp parse_line("inc " <> r), do: {:inc, r}
    defp parse_line("jmp +" <> offset), do: {:jmp, String.to_integer(offset)}
    defp parse_line("jmp -" <> offset), do: {:jmp, 0 - String.to_integer(offset)}

    defp parse_line(<<"jie ", <<r::binary-size(1)>>, ", +", offset::binary>>),
      do: {:jie, r, String.to_integer(offset)}

    defp parse_line(<<"jie ", <<r::binary-size(1)>>, ", -", offset::binary>>),
      do: {:jie, r, 0 - String.to_integer(offset)}

    defp parse_line(<<"jio ", <<r::binary-size(1)>>, ", +", offset::binary>>),
      do: {:jio, r, String.to_integer(offset)}

    defp parse_line(<<"jio ", <<r::binary-size(1)>>, ", -", offset::binary>>),
      do: {:jio, r, 0 - String.to_integer(offset)}
  end

  def p1 do
    input_stream()
    |> Program.new()
    |> Program.run()
    |> Program.fetch!(:b)
  end

  def p2 do
    input_stream()
    |> Program.new()
    |> Map.put(:a, 1)
    |> Program.run()
    |> Program.fetch!(:b)
  end
end
