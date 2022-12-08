use crate::{tests, AoC};

pub struct Day02;

fn score(line: Vec<char>) -> usize {
    match line[..] {
        ['A', 'X'] => 1 + 3,
        ['A', 'Y'] => 2 + 6,
        ['A', 'Z'] => 3,
        ['B', 'X'] => 1,
        ['B', 'Y'] => 2 + 3,
        ['B', 'Z'] => 3 + 6,
        ['C', 'X'] => 1 + 6,
        ['C', 'Y'] => 2,
        ['C', 'Z'] => 3 + 3,
        _ => unreachable!(),
    }
}

fn end(line: Vec<char>) -> usize {
    match line[..] {
        ['A', 'X'] => 3,
        ['A', 'Y'] => 1 + 3,
        ['A', 'Z'] => 2 + 6,
        ['B', 'X'] => 1,
        ['B', 'Y'] => 2 + 3,
        ['B', 'Z'] => 3 + 6,
        ['C', 'X'] => 2,
        ['C', 'Y'] => 3 + 3,
        ['C', 'Z'] => 1 + 6,
        _ => unreachable!(),
    }
}

impl AoC for Day02 {
    type Output = usize;
    type Input = Vec<Vec<char>>;

    fn day() -> usize {
        2
    }

    fn parse(input: &str) -> Self::Input {
        input
            .lines()
            .map(|l| {
                l.chars()
                    .filter(char::is_ascii_alphabetic)
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn part_one(input: Self::Input) -> Self::Output {
        input.into_iter().map(score).sum()
    }

    fn part_two(input: Self::Input) -> Self::Output {
        input.into_iter().map(end).sum()
    }
}

tests!(Day02, 15, 13924, 12, 13448);
