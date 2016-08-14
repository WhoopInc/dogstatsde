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
       version: "0.7.0",
       elixir: "~> 1.2",
       build_embedded: Mix.env == :prod,
       start_permanent: Mix.env == :prod,
       deps: deps]
    end

    # Configuration for the OTP application
    #
    # Type "mix help compile.app" for more information
    def application do
      [
        applications: [:logger, :worker_pool, :stillir],
        mod: {:dogstatsd_app, []}
      ]
    end

    # Dependencies can be Hex packages:
    #
    #   {:mydep, "~> 0.3.0"}
    #
    # Or git/path repositories:
    #
    #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
    #
    # Type "mix help deps" for more examples and options
    defp deps do
      [
        {:stillir, "~> 1.0.0"},
        {:worker_pool, "~> 2.1"},
        {:meck, "~> 0.8.4" , only: :test}
      ]
    end
  end
end
