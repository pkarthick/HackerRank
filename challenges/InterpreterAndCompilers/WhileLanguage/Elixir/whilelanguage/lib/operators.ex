defmodule Operators do
  defprotocol Operator do
    def operate(op1, op2)
  end

  defmodule Arithmetic do
    defmodule Add do
      defstruct symbol: :add
    end

    defmodule Substract do
      defstruct symbol: :sub
    end

    defmodule Multiply do
      defstruct symbol: :mul
    end

    defmodule Divide do
      defstruct symbol: :div
    end
  end

  defmodule Boolean do
    defmodule And do
    end

    defmodule Or do
    end
  end

  defmodule Relational do
    defmodule LessThan do
    end

    defmodule GreaterThan do
    end
  end
end
