defmodule Reader do
  def read do
    case IO.read(:stdio, :line) do
      :eof ->
        ""

      {:error, reason} ->
        IO.puts("Error: #{reason}")

      data ->
        if data |> String.trim() |> String.length() > 0 do
          String.trim_trailing(data) <> read()
        else
          data
        end
    end
  end

  def readAll do
    Reader.read() |> String.trim_trailing()
  end
end
