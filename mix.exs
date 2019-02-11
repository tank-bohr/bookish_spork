defmodule BookishSporkDoc.MixProject do
  use Mix.Project

  def project do
    [
      app: :bookish_spork_doc,
      version: "0.3.3",
      elixir: "~> 1.7",
      deps: deps(),
      compilers: [:app],

      # Docs
      name: "Bookish spork",
      source_url: "https://github.com/tank-bohr/bookish_spork",
      homepage_url: "https://github.com/tank-bohr/bookish_spork",
      docs: [
        logo: "logo.svg",
        main: "readme",
        extras: ["README.md"],
        output: "docs",
        source_beam: ["_build/eep48/lib/bookish_spork/ebin"],
        filter_prefix: false
      ]
    ]
  end

  defp deps do
    [
      {:bookish_spork, path: "_build/eep48/lib/bookish_spork", only: :dev, compile: false, runtime: false},
      {:ex_doc, github: "tank-bohr/ex_doc", branch: :master, only: :dev, runtime: false}
    ]
  end
end
