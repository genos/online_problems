defmodule SecretHandshake do
  use Bitwise, only_operators: true

  @steps %{1 => "wink", 2 => "double blink", 4 => "close your eyes", 8 => "jump"}

  def bits(code) do
    Enum.map(Map.keys(@steps), fn k -> code &&& k end)
    |> Enum.reject(&(&1 == 0))
  end

  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    xs = bits(code) |> Enum.map(&Map.get(@steps, &1))
    if (code &&& 16) == 16 do Enum.reverse(xs) else xs
    end
  end
end
