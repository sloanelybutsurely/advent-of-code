import Config

config :nx, :default_defn_options, compiler: EXLA

config :advent_of_code_utils,
  year: 2015,
  auto_reload?: true,
  session: System.get_env("ADVENT_OF_CODE_SESSION")
