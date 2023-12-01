import Config

config :advent_of_code_utils,
  auto_compile?: true,
  time_calls?: true,
  session: System.get_env("ADVENT_OF_CODE_SESSION")

config :iex, inspect: [charlists: :as_lists]
