defmodule NucleotideCount do
  @nucleotides MapSet.new([?A, ?C, ?G, ?T])

  @spec valid?(char) :: boolean
  def valid?(nucleotide) do
    MapSet.member?(@nucleotides, nucleotide)
  end

  @doc """
  Counts individual nucleotides in a NucleotideCount strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    Enum.count strand, fn x -> valid?(x) && x == nucleotide end
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    strand
    |> Enum.filter(&valid?(&1))
    |> Enum.group_by(& &1)
    |> Enum.map(fn {k, v} -> {k, length(v)} end)
    |> Enum.into(Map.new @nucleotides, &{&1, 0})
  end
end
