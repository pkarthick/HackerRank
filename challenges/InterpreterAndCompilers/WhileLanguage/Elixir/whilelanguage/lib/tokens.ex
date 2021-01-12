defmodule Tokenizer do
  defmacro is_alpha(c) do
    c >= 97 and c <= 122
  end

  def parse([], tokens) do
    Enum.reverse(tokens)
  end

  def parse(' ' ++ rest, tokens) do
    parse(rest, tokens)
  end

  def parse('(' ++ rest, tokens) do
    parse(rest, [:open | tokens])
  end

  def parse(')' ++ rest, tokens) do
    parse(rest, [:close | tokens])
  end

  def parse('+' ++ rest, tokens) do
    parse(rest, [:add | tokens])
  end

  def parse('-' ++ rest, tokens) do
    parse(rest, [:sub | tokens])
  end

  def parse('*' ++ rest, tokens) do
    parse(rest, [:mul | tokens])
  end

  def parse('/' ++ rest, tokens) do
    parse(rest, [:div | tokens])
  end

  def parse('<' ++ rest, tokens) do
    parse(rest, [:lt | tokens])
  end

  def parse('>' ++ rest, tokens) do
    parse(rest, [:gt | tokens])
  end

  def parse([t | rest], tokens) when t >= 48 and t <= 57 do
    num =
      [t | rest |> Enum.take_while(&(&1 >= 48 and &1 <= 57))]
      |> List.to_string()
      |> String.to_integer()

    rest = rest |> Enum.drop_while(&(&1 >= 48 and &1 <= 57))

    parse(rest, [num | tokens])
  end

  def parse([t | rest], tokens) when t >= 97 and t <= 122 do
    is_alp = &(&1 >= 97 and &1 <= 122)

    xvar = [t | rest] |> Enum.take_while(is_alp) |> List.to_string()

    rest = rest |> Enum.drop_while(is_alp)

    parse(rest, [xvar | tokens])
  end
end
