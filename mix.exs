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
       version: version(),
       name: "Dogstatsd",
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
        {:worker_pool, "~> 2.1.0"},
        {:meck, "~> 0.8.4", only: :test}
      ]
    end

    def version do
      # Fetch or fabricate a version number
      {:ok, [{:application, :dogstatsd, appdata}]} = :file.consult("src/dogstatsd.app.src")
      case appdata[:vsn] do
        :git ->
          # Fabricate a magic git version
          {git_tags,0} = System.cmd("git", ["tag", "--sort=version:refname"])
          last_vsn = git_tags |> String.trim |> String.split |> List.last
          {git_hash,0} = System.cmd("git", ["rev-parse", "--short", "HEAD"])
          short_hash = git_hash |> String.trim
          "#{last_vsn}+build-#{short_hash}"
        real_vsn ->
          # We get here when this is a downloaded Hex package
          real_vsn
      end
    end
  end
end
