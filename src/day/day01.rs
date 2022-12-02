use itertools::Itertools;

use crate::{tests, AoC};

pub struct Day01;

impl AoC for Day01 {
    type Output = usize;
    type Input = Vec<Vec<usize>>;

    fn day() -> usize {
        1
    }

    fn parse(input: &str) -> Self::Input {
        input
            .split("\n\n")
            .map(|l| l.lines().map(|t| t.parse().unwrap()).collect())
            .collect()
    }

    fn part_one(input: Self::Input) -> Self::Output {
        input.iter().map(|e| e.iter().sum()).max().unwrap()
    }

    fn part_two(input: Self::Input) -> Self::Output {
        input
            .iter()
            .map(|e| e.iter().sum::<usize>())
            .sorted()
            .rev()
            .take(3)
            .sum()
    }
}

tests!(Day01, 24000, 68442, 45000, 204837);
