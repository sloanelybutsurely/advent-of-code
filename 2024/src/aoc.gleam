import argv
import birl
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import glint

fn day_flag() -> glint.Flag(Int) {
  let flag =
    glint.int_flag("day")
    |> glint.flag_help("The day to run")

  case birl.now() |> birl.get_day() {
    birl.Day(year: _, month: 12, date: date) ->
      flag
      |> glint.flag_default(date)
    _ -> flag
  }
}

fn parts_flag() -> glint.Flag(List(Int)) {
  glint.ints_flag("part")
  |> glint.flag_default([1, 2])
  |> glint.flag_help("The part(s) to run")
}

fn run() -> glint.Command(Nil) {
  use day <- glint.flag(day_flag())
  use parts <- glint.flag(parts_flag())
  use _, _, flags <- glint.command()
  let assert Ok(day) = day(flags)
  let assert Ok(parts) = parts(flags)
  io.println(int.to_string(day))
  parts
  |> list.map(int.to_string)
  |> string.join(", ")
  |> io.println()
}

pub fn main() {
  glint.new()
  |> glint.with_name("run")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: run())
  |> glint.run(argv.load().arguments)
}
