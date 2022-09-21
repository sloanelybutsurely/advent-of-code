defmodule AdventOfCode.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :eex]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:advent_of_code_utils, "~> 1.0"},
      {:benchee, "~> 1.0", only: :dev},
      {:exla, "~> 0.1.0-dev", github: "elixir-nx/nx", sparse: "exla"},
      {:nimble_parsec, "~> 1.0"},
      {:nx, "~> 0.1.0-dev", github: "elixir-nx/nx", sparse: "nx", override: true}
    ]
  end
end
