defmodule WhileLanguage do
  @moduledoc """
  Documentation for WhileLanguage.
  """

  def parseCode(code) do
  end

  def evalCode do
    Reader.readAll() |> parseCode
  end
end
