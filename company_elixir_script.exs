evaluator = IEx.Server.start_evaluator([])
Process.put(:evaluator, evaluator)

defmodule CompanyElixirServer do
  def evaluator do
    {Process.get(:evaluator), self()}
  end

  defp expand(expr) do
    do_expand(expr)
  end

  def expand(expr, alias_lines) do
    if String.ends_with?(expr, ".") do
      clean_expr = String.replace(expr, ".", "")
      aliases = parse_aliases(alias_lines)
      found_alias = Map.get(aliases, clean_expr)

      if found_alias do
        case do_expand(found_alias <> ".") do
          :not_found -> do_expand(expr)
          result -> result
        end
      end
    else
      do_expand(expr)
    end
  rescue
    _ -> :not_found
  end

  defp parse_aliases(alias_lines) do
    alias_lines
    |> Enum.flat_map(fn line ->
      parse_alias(line)
    end)
    |> Enum.into(%{})
  end

  defp parse_alias(alias_line) do
    clean_alias_line =
      alias_line
      |> String.replace("alias", "")
      |> String.trim()

    cond do
      String.contains?(clean_alias_line, "as:") ->
        [aliased_module, name]= String.split(clean_alias_line, ", as:")

        [{String.trim(name), String.trim(aliased_module)}]

      String.contains?(clean_alias_line, "{") ->
        [head | tail] = String.split(clean_alias_line, ["}", "{", ","])

        Enum.map(tail, fn part ->
          {part, head <> part}
        end)

      true ->
        name =
          clean_alias_line
          |> String.split(".")
          |> List.last()

        [{name, clean_alias_line}]
    end
  end

  defp do_expand(expr) when is_binary(expr) do
    charlist = String.to_charlist(expr)

    do_expand(charlist)
  end

  defp do_expand(expr) do
    case IEx.Autocomplete.expand(Enum.reverse(expr), __MODULE__) do
      {:yes, _, result} -> result
      _ -> :not_found
    end
  end
end

IEx.configure(inspect: [limit: :infinity])
Logger.remove_backend(:console)
