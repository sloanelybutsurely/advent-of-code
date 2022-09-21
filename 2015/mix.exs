defmodule AdventOfCode2015.MixProject do
  use Mix.Project

  def project do
    [
      app: :advent_of_code_2015,
      version: "0.1.0",
      elixir: "~> 1.12",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:advent_of_code_utils, "~> 1.0"},
      {:exla, "~> 0.1.0-dev", github: "elixir-nx/nx", sparse: "exla"},
      {:nimble_parsec, "~> 1.0"},
      {:nx, "~> 0.1.0-dev", github: "elixir-nx/nx", sparse: "nx", override: true}
    ]
  end
end
