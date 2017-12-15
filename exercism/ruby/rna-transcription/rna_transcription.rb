class Complement
  TABLE = {?G => ?C, ?C => ?G, ?T => ?A, ?A => ?U}
  def self.of_dna(dna)
    dna.each_char.map { |c| TABLE.fetch(c) { |_| return '' } }.join
  end
end

module BookKeeping
  VERSION = 4
end
