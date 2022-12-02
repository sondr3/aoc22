mod day;

use std::time::Instant;

use day::day01;

#[macro_export]
macro_rules! tests {
    ($day: ident) => {
        #[allow(dead_code)]
        const INPUT: &str = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/inputs/",
            stringify!($day),
            ".input"
        ));
        #[allow(dead_code)]
        const EXAMPLE: &str = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/inputs/",
            stringify!($day),
            ".test"
        ));
    };
    ($day: ident, $p1: expr) => {
        tests!($day);

        #[test]
        fn part_1_example() {
            assert_eq!($p1, $day::part_one($day::parse(EXAMPLE)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr) => {
        tests!($day, $p1);

        #[test]
        fn part_1() {
            assert_eq!($a1, $day::part_one($day::parse(INPUT)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr, $p2: expr) => {
        tests!($day, $p1, $a1);

        #[test]
        fn part_2_example() {
            assert_eq!($p2, $day::part_two($day::parse(EXAMPLE)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr, $p2: expr, $a2: expr) => {
        tests!($day, $p1, $a1, $p2);

        #[test]
        fn part_2() {
            assert_eq!($a2, $day::part_two($day::parse(INPUT)));
        }
    };
}

trait AoC {
    type Output: std::fmt::Display;
    type Input: Clone;

    fn day() -> usize;
    fn parse(input: &str) -> Self::Input;
    fn part_one(input: Self::Input) -> Self::Output;
    fn part_two(input: Self::Input) -> Self::Output;
    fn solve(input: &str) -> String {
        let now = Instant::now();
        let input = Self::parse(input);
        let parse_time = now.elapsed();

        let p1_input = input.clone();
        let p2_input = input;

        let now = Instant::now();
        let p1 = Self::part_one(p1_input);
        let p1_time = now.elapsed();

        let now = Instant::now();
        let p2 = Self::part_two(p2_input);
        let p2_time = now.elapsed();

        format!(
            "Solution for day {}:\n  Parse time: {:?}\n  Part 1: {} in {:?}\n  Part 2: {} in {:?}",
            Self::day(),
            parse_time,
            p1,
            p1_time,
            p2,
            p2_time
        )
    }
}

pub fn run_day(day: usize, input: &str) -> String {
    match day {
        1 => day01::Day01::solve(input),
        _ => "Not implemented".to_string(),
    }
}
