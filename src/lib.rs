mod day;

use std::time::Instant;

use day::*;

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
            assert_eq!($p1, $day::part_one($day::parse_one(EXAMPLE)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr) => {
        tests!($day, $p1);

        #[test]
        fn part_1() {
            assert_eq!($a1, $day::part_one($day::parse_one(INPUT)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr, $p2: expr) => {
        tests!($day, $p1, $a1);

        #[test]
        fn part_2_example() {
            assert_eq!($p2, $day::part_two($day::parse_two(EXAMPLE)));
        }
    };
    ($day: ident, $p1: expr, $a1: expr, $p2: expr, $a2: expr) => {
        tests!($day, $p1, $a1, $p2);

        #[test]
        fn part_2() {
            assert_eq!($a2, $day::part_two($day::parse_two(INPUT)));
        }
    };
}

trait AoC {
    type Output: std::fmt::Display;
    type Input: Clone;

    fn day() -> usize;
    fn parse_one(input: &str) -> Self::Input;
    fn parse_two(input: &str) -> Self::Input {
        Self::parse_one(input)
    }
    fn part_one(input: Self::Input) -> Self::Output;
    fn part_two(input: Self::Input) -> Self::Output;
    fn solve(input: &str) -> String {
        let now = Instant::now();
        let p1_input = Self::parse_one(input);
        let p1_parse_time = now.elapsed();

        let now = Instant::now();
        let p1 = Self::part_one(p1_input);
        let p1_time = now.elapsed();

        let now = Instant::now();
        let p2_input = Self::parse_one(input);
        let p2_parse_time = now.elapsed();

        let now = Instant::now();
        let p2 = Self::part_two(p2_input);
        let p2_time = now.elapsed();

        format!(
            r#"
Solution for day {:02}:
  Part 1: {} in {:?}, parsed in {:?} 
  Part 2: {} in {:?}, parsed in {:?}"#,
            Self::day(),
            p1,
            p1_time,
            p1_parse_time,
            p2,
            p2_time,
            p2_parse_time
        )
    }
}

pub fn run_day(day: usize, input: &str) -> String {
    match day {
        1 => day01::Day01::solve(input),
        2 => day02::Day02::solve(input),
        _ => "Not implemented".to_string(),
    }
}
