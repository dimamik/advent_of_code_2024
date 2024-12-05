use std::collections::{HashMap, HashSet};
use std::fs;

fn parse_rules(rules: &[&str]) -> HashMap<u32, HashSet<u32>> {
    rules.iter().fold(HashMap::new(), |mut map, &rule| {
        let mut parts = rule.split('|').map(|x| x.parse::<u32>().unwrap());
        let key = parts.next().unwrap();
        let val = parts.next().unwrap();
        map.entry(key).or_default().insert(val);
        map
    })
}

fn is_update_ordered(update: &[u32], ordering_rules: &HashMap<u32, HashSet<u32>>) -> bool {
    let mut seen = HashSet::new();
    for &page in update {
        if let Some(deps) = ordering_rules.get(&page) {
            if deps.iter().any(|dep| seen.contains(dep)) {
                return false;
            }
        }
        seen.insert(page);
    }
    true
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input file");
    let mut sections = input.split("\n\n");
    let rules_input: Vec<&str> = sections.next().unwrap().lines().collect();
    let updates_input: Vec<Vec<u32>> = sections.next().unwrap().lines()
        .map(|line| line.split(',').map(|x| x.parse().unwrap()).collect())
        .collect();

    let ordering_rules = parse_rules(&rules_input);

    let total_middle_sum: u32 = updates_input.into_iter()
        .filter(|update| is_update_ordered(update, &ordering_rules))
        .map(|update| update[update.len() / 2])
        .sum();

    println!("{}", total_middle_sum);
}
