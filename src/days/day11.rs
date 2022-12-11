use crate::{tests, AoC};

pub struct Day11;

#[derive(Clone, Debug)]
enum Expr {
    Old,
    Num(usize),
}

impl Expr {
    fn op(&self, old: usize) -> usize {
        match self {
            Expr::Old => old,
            Expr::Num(num) => *num,
        }
    }
}

impl From<&str> for Expr {
    fn from(input: &str) -> Self {
        match input {
            "old" => Expr::Old,
            _ => Expr::Num(input.parse().unwrap()),
        }
    }
}

#[derive(Clone, Debug)]
enum Operation {
    Plus(Expr, Expr),
    Times(Expr, Expr),
}

impl Operation {
    fn level(&self, old: usize) -> usize {
        match self {
            Operation::Plus(e1, e2) => e1.op(old) + e2.op(old),
            Operation::Times(e1, e2) => e1.op(old) * e2.op(old),
        }
    }
}

impl From<&str> for Operation {
    fn from(line: &str) -> Self {
        let binding = line.replace("Operation: new = ", "");
        let line: Vec<_> = binding.split(' ').collect();
        let e1 = Expr::from(line[0]);
        let e2 = Expr::from(line[2]);

        match line[1] {
            "+" => Operation::Plus(e1, e2),
            "*" => Operation::Times(e1, e2),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Monkey {
    items: Vec<usize>,
    operation: Operation,
    test: usize,
    target_true: usize,
    target_false: usize,
}

impl Monkey {
    fn handle_item<F>(&self, func: F) -> Vec<(usize, usize)>
    where
        F: Fn(usize) -> usize + Copy,
    {
        let mut res = Vec::new();

        for item in &self.items {
            let worry_level = func(self.operation.level(*item));

            if worry_level % self.test == 0 {
                res.push((self.target_true, worry_level));
            } else {
                res.push((self.target_false, worry_level));
            }
        }

        res
    }

    fn end_of_round(&mut self) {
        self.items.drain(..);
    }

    fn get_item(&mut self, item: usize) {
        self.items.push(item);
    }

    fn parse(lines: Vec<&str>) -> Self {
        let lines = &lines[1..];
        let items = lines[0]
            .replace("Starting items: ", "")
            .split(", ")
            .map(|l| l.parse().unwrap())
            .collect();
        let operation = Operation::from(lines[1]);
        let test = lines[2].replace("Test: divisible by ", "").parse().unwrap();
        let target_true = lines[3]
            .replace("If true: throw to monkey ", "")
            .parse()
            .unwrap();
        let target_false = lines[4]
            .replace("If false: throw to monkey ", "")
            .parse()
            .unwrap();

        Monkey {
            items,
            operation,
            test,
            target_true,
            target_false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MonkeyBusiness {
    monkeys: Vec<Monkey>,
    current_monkey: usize,
    inspected: Vec<usize>,
}

impl MonkeyBusiness {
    fn solve<F>(&mut self, rounds: usize, func: F) -> usize
    where
        F: Fn(usize) -> usize + Copy,
    {
        self.run_rounds(rounds, func);
        self.inspected.sort_unstable();
        self.inspected.iter().rev().take(2).product()
    }

    fn run_rounds<F>(&mut self, count: usize, func: F)
    where
        F: Fn(usize) -> usize + Copy,
    {
        for _ in 0..count {
            for _ in 0..self.monkeys.len() {
                self.round(func);
            }
        }
    }

    fn round<F>(&mut self, func: F)
    where
        F: Fn(usize) -> usize + Copy,
    {
        let monkey = &mut self.monkeys[self.current_monkey];
        let res = monkey.handle_item(func);
        self.inspected[self.current_monkey] += res.len();
        monkey.end_of_round();

        res.into_iter()
            .for_each(|(target, item)| self.monkeys[target].get_item(item));
        self.current_monkey = (self.current_monkey + 1) % self.monkeys.len();
    }
}

fn modulo(val: usize, modulo: usize) -> usize {
    val % modulo
}

fn div(val: usize, div: usize) -> usize {
    val / div
}

impl AoC for Day11 {
    type Output = usize;
    type Input = MonkeyBusiness;

    fn day() -> usize {
        11
    }

    fn parse(input: &str) -> Self::Input {
        let monkeys: Vec<_> = input
            .split("\n\n")
            .map(|l| l.lines().map(str::trim).collect())
            .map(Monkey::parse)
            .collect();

        MonkeyBusiness {
            current_monkey: 0,
            inspected: vec![0; monkeys.len()],
            monkeys,
        }
    }

    fn part_one(mut input: Self::Input) -> Self::Output {
        input.solve(20, move |x| div(x, 3))
    }

    fn part_two(mut input: Self::Input) -> Self::Output {
        let prod = input.monkeys.iter().map(|m| m.test).product();
        input.solve(10_000, move |x| modulo(x, prod))
    }
}

tests!(Day11, 10605, 62491, 2713310158, 17408399184);
