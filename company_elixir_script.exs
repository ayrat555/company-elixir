evaluator = IEx.Server.start_evaluator([])
Process.put(:evaluator, evaluator)

defmodule CompanyElixirServer do
  def evaluator do
    {Process.get(:evaluator), self()}
  end

  def expand(expr) do
    case IEx.Autocomplete.expand(Enum.reverse(expr), __MODULE__) do
      {:yes, _, result} -> result
      _ -> :not_found
    end
  end
end

IEx.configure(inspect: [limit: :infinity])
Logger.remove_backend(:console)
