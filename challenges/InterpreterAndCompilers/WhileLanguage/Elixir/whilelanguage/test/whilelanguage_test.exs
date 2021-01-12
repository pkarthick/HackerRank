defmodule WhilelanguageTest do
  use ExUnit.Case
  doctest Whilelanguage

  test "greets the world" do
    assert Whilelanguage.hello() == :world
  end
end
