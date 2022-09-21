# Advent of Code 2021

<details>
  <summary>Setup</summary>

  Using [asdf](https://asdf-vm.com/#/):

  ```sh
  asdf plugin add erlang
  asdf plugin add elixir
  asdf install
  ```
</details>

|  S  |  M  |  T  |  W  |  T  |  F  |  S  |
| :-: | :-: | :-: | :-: | :-: | :-: | :-: |
|     |     |     |  1  |  2  |  3  |  4  |
|  5  |  6  |  7  |  8  |  9  | 10  | 11  |
| 12  | 13  | 14  | 15  | 16  | 17  | 18  |
| 19  | 20  | 21  | 22  | 23  | 24  | 25  |

---

## Mix Tasks

- `advent_of_code.gen.solution`
- `advent_of_code.solve`
- `advent_of_code.fetch_input`

Run `mix help <task>` for details.

## [`AdventOfCode.PuzzleSolver`](./lib/advent_of_code/puzzle_solver.ex)

A behaviour for a solution to a puzzle. Must define a `solve/2` callback.

## [`AdventOfCode.PuzzleCase`](./test/support/puzzle_case.ex)

Case template defining an `assert_solution/2` helper.

<!-- links -->
