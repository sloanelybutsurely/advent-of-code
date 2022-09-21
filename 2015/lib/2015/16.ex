import AOC

aoc 2015, 16 do
  @moduledoc """
      children: 3
      cats: 7
      samoyeds: 2
      pomeranians: 3
      akitas: 0
      vizslas: 0
      goldfish: 5
      trees: 3
      cars: 2
      perfumes: 1
  """

  def known_properties() do
    """
    children: 3
    cats: 7
    samoyeds: 2
    pomeranians: 3
    akitas: 0
    vizslas: 0
    goldfish: 5
    trees: 3
    cars: 2
    perfumes: 1
    """
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ": "))
    |> Enum.map(fn [l, v] -> {l, String.to_integer(v)} end)
  end

  def get_value(sue, label) do
    [value] = Regex.run(~r/#{label}: (\d+)/, sue, capture: :all_but_first)
    value |> String.to_integer()
  end

  def p1 do
    input_stream()
    |> Stream.filter(fn sue ->
      known_properties()
      |> Enum.filter(fn {label, _} -> String.contains?(sue, label) end)
      |> Enum.all?(fn {label, value} ->
        if String.contains?(sue, label) do
          String.contains?(sue, "#{label}: #{value}")
        else
          true
        end
      end)
    end)
    |> Stream.take(1)
    |> Enum.to_list()
    |> hd()
  end

  def p2 do
    input_stream()
    |> Stream.filter(fn sue ->
      known_properties()
      |> Enum.filter(fn {label, _} -> String.contains?(sue, label) end)
      |> Enum.all?(fn
        {label, value} when label in ~w[cats trees] ->
          IO.inspect(label, label: "label")
          IO.inspect(value, label: "value")
          IO.inspect(get_value(sue, label), label: "get_value(sue, label)")
          value < get_value(sue, label)

        {label, value} when label in ~w[goldfish pomeranians] ->
          value > get_value(sue, label)

        {label, value} ->
          String.contains?(sue, "#{label}: #{value}")
      end)
    end)
    |> Enum.to_list()
  end
end
