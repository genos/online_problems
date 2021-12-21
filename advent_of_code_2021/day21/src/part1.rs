struct Player {
    position: u64,
    score: u64,
}

impl Player {
    fn won(&self) -> bool {
        self.score >= 1000
    }
    fn move_spaces(&mut self, spaces: u64) {
        self.position += spaces % 10;
        while self.position > 10 {
            self.position -= 10;
        }
        self.score += self.position;
    }
}

pub struct Game {
    player1: Player,
    player2: Player,
    ones_turn: bool,
}

impl Game {
    pub fn new(pos1: u64, pos2: u64) -> Self {
        Game {
            player1: Player {
                position: pos1,
                score: 0,
            },
            player2: Player {
                position: pos2,
                score: 0,
            },
            ones_turn: true,
        }
    }
    pub fn play(&mut self) -> u64 {
        let mut die = (1..=100).cycle();
        let mut num_rolls = 0;
        while !self.player1.won() && !self.player2.won() {
            num_rolls += 3;
            let step = (0..3).flat_map(|_| die.next()).sum();
            if self.ones_turn {
                self.player1.move_spaces(step);
            } else {
                self.player2.move_spaces(step);
            }
            self.ones_turn = !self.ones_turn;
        }
        num_rolls * self.player1.score.min(self.player2.score)
    }
}
