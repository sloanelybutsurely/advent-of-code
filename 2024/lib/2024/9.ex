import AOC
import AOC.Prelude

aoc 2024, 9 do
  require Integer

  def p1(input) do
    input
    |> read_memory_tuple()
    |> defrag_memory()
    |> checksum()
  end

  def p2(input) do
    input
    |> read_blocks_tuple()
    |> defrag_blocks()
    |> blocks_to_memory()
    |> checksum()
  end

  ## part 1

  defp defrag_memory(memory) do
    defrag_memory(memory, 0, tuple_size(memory) - 1)
  end

  defp defrag_memory(memory, l, r) when l >= r, do: memory

  defp defrag_memory(memory, l, r) when elem(memory, l) == nil and elem(memory, r) != nil do
    memory
    |> put_elem(l, elem(memory, r))
    |> put_elem(r, nil)
    |> defrag_memory(l + 1, r - 1)
  end

  defp defrag_memory(memory, l, r) when elem(memory, l) != nil, do: defrag_memory(memory, l + 1, r)
  defp defrag_memory(memory, l, r) when elem(memory, r) == nil, do: defrag_memory(memory, l, r - 1)

  defp checksum(memory) do
    for i <- 0..(tuple_size(memory) - 1), reduce: 0 do
      sum ->
        el = elem(memory, i)

        if is_nil(el) do
          sum
        else
          sum + el * i
        end
    end
  end

  ## part 2

  defp defrag_blocks(blocks), do: defrag_blocks(blocks, tuple_size(blocks) - 1)
  defp defrag_blocks(blocks, i) when i < 0, do: blocks
  defp defrag_blocks(blocks, i) when elem(elem(blocks, i), 0) == :free, do: defrag_blocks(blocks, i - 1)

  defp defrag_blocks(blocks, i) do
    {:file, _id, file_size} = file = elem(blocks, i)

    free_idx = Enum.find(0..i, &match?({:free, free_size} when free_size >= file_size, elem(blocks, &1)))

    blocks =
      if is_nil(free_idx) do
        # no space for that block, do nothing
        blocks
      else
        {:free, free_size} = elem(blocks, free_idx)

        blocks =
          blocks
          |> put_elem(free_idx, file)
          |> put_elem(i, {:free, file_size})

        if file_size == free_size do
          blocks
        else
          new_free_block = {:free, free_size - file_size}

          Tuple.insert_at(blocks, free_idx + 1, new_free_block)
        end
      end

    blocks
    |> merge_blocks()
    |> defrag_blocks(i - 1)
  end

  defp merge_blocks(blocks) do
    for i <- (tuple_size(blocks) - 2)..0//-1, reduce: blocks do
      blocks ->
        left = elem(blocks, i)
        right = elem(blocks, i + 1)

        case {left, right} do
          {{:free, left_size}, {:free, right_size}} ->
            blocks
            |> put_elem(i, {:free, left_size + right_size})
            |> Tuple.delete_at(i + 1)

          {{:file, id, left_size}, {:file, id, right_size}} ->
            blocks
            |> put_elem(i, {:file, id, left_size + right_size})
            |> Tuple.delete_at(i + 1)

          _ ->
            blocks
        end
    end
  end

  defp blocks_to_memory(blocks) do
    blocks
    |> Tuple.to_list()
    |> Enum.flat_map(fn
      {:free, n} -> List.duplicate(nil, n)
      {:file, id, n} -> List.duplicate(id, n)
    end)
    |> List.to_tuple()
  end

  ## input 

  defp read_memory_tuple(input) do
    input
    |> ints("")
    |> Enum.with_index()
    |> Enum.flat_map(fn {n, i} ->
      if Integer.is_even(i) do
        id = div(i, 2)
        List.duplicate(id, n)
      else
        List.duplicate(nil, n)
      end
    end)
    # because memory is a fixed size we can just read it into a tuple for
    # constant time random access
    |> List.to_tuple()
  end

  defp read_blocks_tuple(input) do
    input
    |> ints("")
    |> Enum.with_index()
    |> Enum.map(fn {n, i} ->
      if Integer.is_even(i) do
        {:file, div(i, 2), n}
      else
        {:free, n}
      end
    end)
    |> List.to_tuple()
  end
end
