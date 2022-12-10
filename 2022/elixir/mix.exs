defmodule Aoc2022.MixProject do
  use Mix.Project

  def project do
    [
      app: :aoc_2022,
      version: "0.1.0",
      elixir: "~> 1.14",
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
      {:advent_of_code_utils, "~> 3.1"}
    ]
  end
end
