import Config

config :advent_of_code_utils,
  session: System.fetch_env!("AOC_SESSION"),
  auto_compile?: true,
  time_calls?: true

config :iex, inspect: [charlists: :as_lists]
