defmodule SecretHandshake do

  use Bitwise, only_operators: true

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
    ["wink", "double blink", "close your eyes", "jump"]
    |> Enum.with_index
    |> Enum.filter(fn {_, i} -> (code &&& (1 <<< i)) == (1 <<< i) end)
    |> Enum.map(&(elem(&1, 0)))
    |> (fn ys -> if (code &&& 16) == 16 do Enum.reverse(ys) else ys end end).()
  end

end
