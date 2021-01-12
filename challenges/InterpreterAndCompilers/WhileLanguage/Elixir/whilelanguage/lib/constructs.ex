defmodule Constructs do
  defprotocol Construct do
    def eval(c)
    def new(s)
  end

  defmodule Assignment do
    defstruct variable: String, expression: Expresions.Binary
  end

  defmodule While do
    defstruct pred: Expressions.Boolean, constructs: []
  end

  defmodule If do
    defstruct pred: Expressions.Boolean, trueConstructs: [], falseConstructs: []
  end

  defimpl Construct, for: Assignment do
    def new(_) do
      %Assignment{}
    end

    def eval(_) do
      nil
    end
  end

  def parse(' ' ++ s, constructs) do
    parse(s, constructs)
  end

  def parse('\t' ++ s, constructs) do
    parse(s, constructs)
  end

  def parse(';' ++ s, constructs) do
    parse(s, constructs)
  end

  def parse('while ' ++ s, constructs) do
    parts = Regex.named_captures(~r{while\s*\(\s*(?<cond>[^\\)]+).*do\s*\{(?<st>.*)\}\s*}, s)

    parts["cond"]
    parts["st"]
  end

  def parse('if ' ++ s, constructs) do
  end

  def parse(s, constructs) do
  end
end
