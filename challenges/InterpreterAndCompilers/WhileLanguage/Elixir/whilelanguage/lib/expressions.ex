defmodule Expresions do
  defmodule Arithmetic do
    defmodule Const do
      defstruct value: nil

      def new(value), do: %Const{value: value}
    end

    defmodule Variable do
      defstruct name: String

      def new(name), do: %Variable{name: name}
    end

    defmodule Binary do
      defstruct first: Binary, op: Operator, second: Binary
    end
  end

  defmodule Boolean do
  end
end
