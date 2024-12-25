module main

import os

struct Direction {
    node string
    dir  string
}

struct Pair {
    node string
    path []string
}

fn min_in_i64_array(arr []i64) i64 {
    if arr.len == 0 {
        return 999999999999999999
    }
    mut m := arr[0]
    for x in arr {
        if x < m {
            m = x
        }
    }
    return m
}

__global (
    n_pad = {
        '0': [Direction{'2','^'}, Direction{'A','>'}],
        '1': [Direction{'2','>'}, Direction{'4','^'}],
        '2': [Direction{'0','v'}, Direction{'1','<'}, Direction{'3','>'}, Direction{'5','^'}],
        '3': [Direction{'2','<'}, Direction{'6','^'}, Direction{'A','v'}],
        '4': [Direction{'1','v'}, Direction{'5','>'}, Direction{'7','^'}],
        '5': [Direction{'2','v'}, Direction{'4','<'}, Direction{'6','>'}, Direction{'8','^'}],
        '6': [Direction{'3','v'}, Direction{'5','<'}, Direction{'9','^'}],
        '7': [Direction{'4','v'}, Direction{'8','>'}],
        '8': [Direction{'5','v'}, Direction{'7','<'}, Direction{'9','>'}],
        '9': [Direction{'6','v'}, Direction{'8','<'}],
        'A': [Direction{'0','<'}, Direction{'3','^'}],
    }
    d_pad = {
        '^': [Direction{'A','>'}, Direction{'v','v'}],
        '<': [Direction{'v','>'}],
        'v': [Direction{'<','<'}, Direction{'^','^'}, Direction{'>','>'}],
        '>': [Direction{'v','<'}, Direction{'A','^'}],
        'A': [Direction{'^','<'}, Direction{'>','v'}],
    }
    pads = [n_pad, d_pad]
)

fn parse_input() []string {
    data := os.read_file('input.txt') or {
        return []string{}
    }
    return data.split_into_lines()
}

fn pairwise(seq string) [][]string {
    mut pairs := [][]string{}
    for i := 0; i < seq.len - 1; i++ {
        pairs << [seq[i..i+1], seq[i+1..i+2]]
    }
    return pairs
}

fn bfs(u string, v string, g &map[string][]Direction) []string {
    mut q := []Pair{}
    mut seen := map[string]bool{}
    mut res := []string{}
    q << Pair{node: u, path: []string{}}
    seen[u] = true
    mut shortest := -1
    for q.len > 0 {
        front := q[0]
        q.delete(0)
        cur := front.node
        path := front.path
        if cur == v {
            if shortest == -1 {
                shortest = path.len
            }
            if path.len == shortest {
                mut final_path := path.clone()
                final_path << 'A'
                res << final_path.join('')
            }
            continue
        }
        if shortest != -1 && path.len >= shortest {
            continue
        }
        if cur in g {
            unsafe {
                for dir in g[cur] {
                    seen[dir.node] = true
                    mut new_path := path.clone()
                    new_path << dir.dir
                    q << Pair{node: dir.node, path: new_path}
                }
            }
        }
    }
    return res
}

struct DfsKey {
    seq   string
    level int
    i     int
}

__global (
    dfs_memo = map[string]i64{}
)

fn make_key(k DfsKey) string {
    return '${k.seq}#${k.level}#${k.i}'
}

fn dfs(seq_ string, level int, i int) i64 {
    mut seq := seq_
    key := make_key(DfsKey{seq, level, i})
    if key in dfs_memo {
        return dfs_memo[key]
    }
    g := &pads[i]
    seq = 'A' + seq
    mut res := i64(0)
    for pair in pairwise(seq) {
        u := pair[0]
        v := pair[1]
        paths := bfs(u, v, g)
        if level == 0 {
            mut lengths := []i64{}
            for p in paths {
                lengths << i64(p.len)
            }
            res += min_in_i64_array(lengths)
        } else {
            mut sub_costs := []i64{}
            for p in paths {
                sub_costs << dfs(p, level - 1, 1)
            }
            res += min_in_i64_array(sub_costs)
        }
    }
    dfs_memo[key] = res
    return res
}

fn part_one(codes []string) i64 {
    mut total := i64(0)
    for code in codes {
        if code.len >= 3 {
            val := code[0..3].i64()
            total += dfs(code, 2, 0) * val
        }
    }
    return total
}

fn part_two(codes []string) i64 {
    mut total := i64(0)
    for code in codes {
        if code.len >= 3 {
            val := code[0..3].i64()
            total += dfs(code, 25, 0) * val
        }
    }
    return total
}

fn main() {
    codes := parse_input()
    println('Part 1: ${part_one(codes)}')
    println('Part 2: ${part_two(codes)}')
}
