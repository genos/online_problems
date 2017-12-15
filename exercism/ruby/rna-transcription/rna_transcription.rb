# My submission to exercism.io's "RNA Transcription" exercise
class Complement
  TABLE = { ?G => ?C, ?C => ?G, ?T => ?A, ?A => ?U }.freeze
  def self.of_dna(dna)
    dna.each_char.map { |c| TABLE[c] || (return '') }.join
  end
end

module BookKeeping
  VERSION = 4
end
