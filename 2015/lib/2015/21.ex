import AOC

aoc 2015, 21 do
  defmodule Weapon do
    defstruct [:damage, :cost]
    def damage(%Weapon{damage: damage}), do: damage
    def damage(nil), do: 0
    def cost(%Weapon{cost: cost}), do: cost
    def cost(nil), do: 0
  end

  defmodule Armor do
    defstruct [:defence, :cost]
    def defence(%Armor{defence: defence}), do: defence
    def defence(nil), do: 0
    def cost(%Armor{cost: cost}), do: cost
    def cost(nil), do: 0
  end

  defmodule Ring do
    defstruct damage: 0, defence: 0, cost: 0
    def defence(%Ring{defence: defence}), do: defence || 0
    def defence(nil), do: 0
    def damage(%Ring{damage: damage}), do: damage || 0
    def damage(nil), do: 0
    def cost(%Ring{cost: cost}), do: cost
    def cost(nil), do: 0
  end

  defmodule Character do
    defstruct [:hp, :weapon, :armor, rings: []]

    def cost(%Character{} = char) do
      Weapon.cost(char.weapon) + Armor.cost(char.armor) +
        (Enum.map(char.rings, &Ring.cost/1) |> Enum.sum())
    end

    def damage(%Character{rings: rings} = char) do
      Weapon.damage(char.weapon) + (Enum.map(rings, &Ring.damage/1) |> Enum.sum())
    end

    def defence(%Character{rings: rings} = char) do
      Armor.defence(char.armor) + (Enum.map(rings, &Ring.defence/1) |> Enum.sum())
    end

    def equip(%Character{weapon: nil} = char, %Weapon{} = weapon) do
      %Character{char | weapon: weapon}
    end

    def equip(%Character{armor: nil} = char, %Armor{} = armor) do
      %Character{char | armor: armor}
    end

    def equip(%Character{rings: []} = char, %Ring{} = ring) do
      %Character{char | rings: [ring]}
    end

    def equip(%Character{rings: [ring_1]} = char, %Ring{} = ring_2) do
      %Character{char | rings: [ring_1, ring_2]}
    end

    def equip(%Character{} = char, nil), do: char

    def attack(%Character{} = attacker, %Character{} = defender) do
      damage = damage(attacker)
      defence = defence(defender)
      %Character{defender | hp: defender.hp - max(1, damage - defence)}
    end

    def defeated?(%Character{hp: hp}), do: hp <= 0
  end

  def p1 do
    boss = boss()

    for weapon <- weapons(),
        armor <- [nil | armor()],
        ring_1 <- [nil | rings()],
        ring_2 <- [nil | rings() -- [ring_1]],
        reduce: :infinity do
      min_cost ->
        player =
          %Character{hp: 100}
          |> Character.equip(weapon)
          |> Character.equip(armor)
          |> Character.equip(ring_1)
          |> Character.equip(ring_2)

        curr_cost = Character.cost(player)

        if curr_cost < min_cost do
          case battle(player, boss) do
            {:win, _} -> curr_cost
            {:lose, _} -> min_cost
          end
        else
          min_cost
        end
    end
  end

  def p2 do
    boss = boss()

    for weapon <- weapons(),
        armor <- [nil | armor()],
        ring_1 <- [nil | rings()],
        ring_2 <- [nil | rings() -- [ring_1]],
        reduce: 0 do
      max_cost ->
        player =
          %Character{hp: 100}
          |> Character.equip(weapon)
          |> Character.equip(armor)
          |> Character.equip(ring_1)
          |> Character.equip(ring_2)

        curr_cost = Character.cost(player)

        if max_cost < curr_cost do
          case battle(player, boss) do
            {:win, _} -> max_cost
            {:lose, _} -> curr_cost
          end
        else
          max_cost
        end
    end
  end

  defp battle(player, boss) do
    player_attack = fn {player, boss} ->
      boss = Character.attack(player, boss)

      if Character.defeated?(boss) do
        {:halt, {:win, player}}
      else
        {:cont, {player, boss}}
      end
    end

    boss_attack = fn {player, boss} ->
      player = Character.attack(boss, player)

      if Character.defeated?(player) do
        {:halt, {:lose, player}}
      else
        {:cont, {player, boss}}
      end
    end

    Stream.cycle([player_attack, boss_attack])
    |> Enum.reduce_while({player, boss}, fn turn, characters ->
      turn.(characters)
    end)
  end

  defp boss do
    %Character{hp: 103, weapon: %Weapon{damage: 9}, armor: %Armor{defence: 2}}
  end

  defp weapons do
    [
      %Weapon{cost: 8, damage: 4},
      %Weapon{cost: 10, damage: 5},
      %Weapon{cost: 25, damage: 6},
      %Weapon{cost: 40, damage: 7},
      %Weapon{cost: 74, damage: 8}
    ]
  end

  defp armor do
    [
      %Armor{cost: 13, defence: 1},
      %Armor{cost: 31, defence: 2},
      %Armor{cost: 53, defence: 3},
      %Armor{cost: 75, defence: 4},
      %Armor{cost: 102, defence: 5}
    ]
  end

  defp rings do
    [
      %Ring{cost: 25, damage: 1},
      %Ring{cost: 50, damage: 2},
      %Ring{cost: 100, damage: 3},
      %Ring{cost: 20, defence: 1},
      %Ring{cost: 40, defence: 2},
      %Ring{cost: 80, defence: 3}
    ]
  end
end
