use std::collections::HashMap;
use std::fs;
use std::path::Path;
#[macro_use]
extern crate scan_fmt;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Player {
    position: u64,
    score: u64,
}

impl Player {
    fn new(position: u64) -> Self {
        Self { position, score: 0 }
    }
    fn move_spaces(&mut self, spaces: u64) {
        self.position += spaces % 10;
        if self.position > 10 {
            self.position -= 10;
        }
        self.score += self.position;
    }
}

fn part1(pos1: u64, pos2: u64) -> u64 {
    let mut player1 = Player::new(pos1);
    let mut player2 = Player::new(pos2);
    let mut die = 0;
    let mut num_rolls = 0;
    while player1.score < 1000 && player2.score < 1000 {
        num_rolls += 3;
        let mut step = 0;
        for _ in 0..3 {
            die += 1;
            if die > 100 {
                die -= 100;
            }
            step += die;
        }
        player1.move_spaces(step);
        std::mem::swap(&mut player1, &mut player2);
    }
    num_rolls * player1.score.min(player2.score)
}

fn part2(pos1: u64, pos2: u64) -> u64 {
    fn go(
        players: (Player, Player),
        cache: &mut HashMap<(Player, Player), (u64, u64)>,
    ) -> (u64, u64) {
        if players.0.score >= 21 {
            (1, 0)
        } else if players.1.score >= 21 {
            (0, 1)
        } else if let Some(&score) = cache.get(&players) {
            score
        } else {
            let (p1, p2) = players;
            let (mut s1, mut s2) = (0, 0);
            for i in 1..4 {
                for j in 1..4 {
                    for k in 1..4 {
                        let mut p1_fut = p1.clone();
                        p1_fut.move_spaces(i + j + k);
                        let (s2_fut, s1_fut) = go((p2.clone(), p1_fut), cache);
                        s1 += s1_fut;
                        s2 += s2_fut;
                    }
                }
            }
            cache.insert((p1, p2), (s1, s2));
            (s1, s2)
        }
    }
    let (s1, s2) = go((Player::new(pos1), Player::new(pos2)), &mut HashMap::new());
    s1.max(s2)
}

fn main() {
    let input = fs::read_to_string(&Path::new("input.txt")).expect("Can't read file!");
    let (pos1, pos2) = scan_fmt!(
        &input,
        "Player 1 starting position: {d}\nPlayer 2 starting position: {d}",
        u64,
        u64
    )
    .expect("Can't parse positions!");
    println!("Part 1: {}", part1(pos1, pos2));
    println!("Part 2: {}", part2(pos1, pos2));
}
