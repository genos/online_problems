defmodule NucleotideCount do
  @nucleotides MapSet.new([?A, ?C, ?G, ?T])

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
    strand
    |> Enum.count(&(MapSet.member?(@nucleotides, &1) && &1 == nucleotide))
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
    |> Enum.filter(&MapSet.member?(@nucleotides, &1))
    |> List.foldl(
      Map.new(@nucleotides, &{&1, 0}),
      fn x, acc -> Map.update!(acc, x, &(&1 + 1)) end
    )
  end
end
