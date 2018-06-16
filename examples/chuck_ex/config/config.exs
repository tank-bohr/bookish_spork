use Mix.Config

config :chuck_norris_api,
  host: "api.chucknorris.io",
  proto: "https"

import_config "#{Mix.env}.exs"
