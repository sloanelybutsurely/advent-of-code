import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import stdin.{stdin}

type Report =
  List(Int)

pub fn main() {
  let reports = stdin() |> iterator.to_list() |> list.map(with: parse_report)

  let part1 = list.count(reports, is_safe) |> int.to_string()
  let part2 = list.count(reports, is_safe_with_damper) |> int.to_string()

  io.print("Part 1: ")
  io.println(part1)
  io.print("Part 2: ")
  io.println(part2)
}

fn is_safe(report: Report) -> Bool {
  let diffs =
    report
    |> list.window_by_2()
    |> list.map(fn(p) { pair.first(p) - pair.second(p) })

  list.all(diffs, fn(n) { 1 <= n && n <= 3 })
  || list.all(diffs, fn(n) { -3 <= n && n <= -1 })
}

fn is_safe_with_damper(report: Report) -> Bool {
  is_safe(report)
  || report
  |> reports_with_a_single_level_removed()
  |> list.any(is_safe)
}

fn reports_with_a_single_level_removed(report: Report) -> List(Report) {
  let len = list.length(report)
  let idxs = list.range(0, len - 1)

  use i <- list.map(idxs)
  let assert #(head, [_, ..tail]) = list.split(report, i)
  list.append(head, tail)
}

fn parse_report(str: String) -> Report {
  str
  |> string.trim()
  |> string.split(" ")
  |> list.map(int.parse)
  |> result.all()
  |> result.unwrap([])
}
