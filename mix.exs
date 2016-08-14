defmodule Dogstatsd do
  [:gauge, :increment, :counter, :histogram, :timing, :timer, :set] |> (Enum.each fn name ->
    def unquote(name)(metric_name, metric_value) do
      apply(:dogstatsd, unquote(name), [metric_name, metric_value])
    end
    def unquote(name)(metric_name, metric_value, rate_or_tags) do
      apply(:dogstatsd, unquote(name), [metric_name, metric_value, rate_or_tags])
    end
    def unquote(name)(metric_name, metric_value, metric_rate, metric_tags) do
      apply(:dogstatsd, unquote(name), [metric_name, metric_value, metric_rate, metric_tags])
    end
  end)
  def start do
    Application.ensure_all_started(:dogstatsd)
  end

  defmodule Mixfile do
    use Mix.Project

    def project do
      [app: :dogstatsd,
       name: "Dogstatsd",
       version: "0.0.0-fill-me-in",
       elixir: "~> 1.2",
       source_url: "https://github.com/WhoopInc/dogstatsde",
       docs: [
         extras: ["README.md"],
         main: "README.md",
       ],
       build_embedded: Mix.env == :prod,
       start_permanent: Mix.env == :prod,
       deps: deps]
    end

    def application do
      [
        applications: [:logger, :worker_pool, :stillir],
        mod: {:dogstatsd_app, []}
      ]
    end

    defp deps do
      [
        {:stillir, "~> 1.0.0"},
        {:worker_pool, "~> 1.0.4"},
        {:meck, "~> 0.8.4", only: :test}
      ]
    end
  end
end
