import Config

config :aoc, session_cookie: System.fetch_env!("SESSION_COOKIE")
