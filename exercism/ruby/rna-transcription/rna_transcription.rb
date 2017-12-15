# My submission to exercism.io's "RNA Transcription" exercise
class Complement
  @table = { ?G => ?C, ?C => ?G, ?T => ?A, ?A => ?U }
  def self.of_dna(dna)
    dna.each_char.map do |c|
      raise(ArgumentError, "bad char: #{c}") unless @table.member? c
      @table[c]
    end.join
  rescue ArgumentError
    ''
  end
end

module BookKeeping
  VERSION = 4
end
