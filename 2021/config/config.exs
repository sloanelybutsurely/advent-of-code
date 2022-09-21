import Config

config :nx, :default_defn_options, compiler: EXLA, client: :cuda

config :advent_of_code_utils,
  session: System.get_env("ADVENT_OF_CODE_SESSION"),
  auto_compile?: true
