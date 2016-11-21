# ElixirExample

Example of some Elixir code using the Dogstatsd library

## Mix.exs

`:dogstatsd` is listed as an application dependency so it will start before your application.

```
  def application do
    [applications: [:logger, :dogstatsd]]
  end
```

`:dogstatsd` is listed as a dependency so Mix will fetch it. (You will actually want to depend on `{:dogstatsd, "~> <version>", hex: :dogstatsde}`, not a relative path!)

```
  defp deps do
    [
      {:dogstatsd, path: "../../"}
    ]
  end
```
