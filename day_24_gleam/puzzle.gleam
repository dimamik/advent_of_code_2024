import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Operator {
  AND
  OR
  XOR
}

pub type Expression {
  Expression(left: String, operator: Operator, right: String, variable: String)
}

// ######## Essential helpers I won't be able to live without ########

/// Unpacks Okay values and raises on errors, mapping Gleam to Elixir ;)
fn unpack(value) {
  case value {
    Ok(value) -> value
    Error(_error) -> panic as "Error unpacking value"
  }
}

fn read_file(path) {
  simplifile.read(path) |> unpack()
}

// ######## End of essential helpers ########

// ######## Types benefits (or not) ########

fn do_operation(operator, left, right) {
  case operator {
    AND -> left && right
    OR -> left || right
    XOR -> left != right
  }
}

fn string_to_operator(operator: String) -> Operator {
  case operator {
    "AND" -> AND
    "OR" -> OR
    "XOR" -> XOR
    _ -> panic as "Unsupported operator"
  }
}

fn bool_to_string(value) {
  case value {
    True -> "1"
    False -> "0"
  }
}

fn int_to_string(int: Int) {
  case int <= 9 {
    True -> "0" <> int.to_string(int)
    False -> int.to_string(int)
  }
}

fn int_to_boolean(value) {
  case value {
    "0" -> False
    "1" -> True
    _ -> panic as "Unsupported value in input"
  }
}

// ######## End of types benefits (or not) ########

fn parse_input() {
  let file = read_file("input.txt")

  let initial_values = file |> string.split("\n\n") |> list.first() |> unpack()

  let sequence = file |> string.split("\n\n") |> list.last() |> unpack()

  let input_map =
    initial_values
    |> string.trim()
    |> string.split("\n")
    |> list.filter(fn(line) { line != "" })
    |> list.map(fn(line) {
      case
        line
        |> string.trim()
        |> string.split(": ")
      {
        [a, b] -> #(a, int_to_boolean(b))
        _ -> panic as "Unsupported value in input"
      }
    })
    |> dict.from_list()

  let expressions =
    sequence
    |> string.trim()
    |> string.split("\n")
    |> list.filter(fn(line) { line != "" })
    |> list.map(fn(line) {
      let line = string.trim(line)

      // There is a "bug" in gleam, so I can't return a const list of size 2
      // And I need to cast it to a tuple
      let #(operators, new_var) = case string.split(line, " -> ") {
        [operators, new_var] -> #(operators, new_var)
        _other -> panic as "AAAAAA"
      }

      let #(a, b, c) = case string.split(operators, " ") {
        [a, b, c] -> #(a, b, c)
        _ ->
          panic as "I ASSURE YOU THAT THIS STRING WILL ALWAYS HAVE 3 ELEMENTS xD"
      }

      Expression(
        left: a,
        operator: string_to_operator(b),
        right: c,
        variable: new_var,
      )
    })

  #(input_map, expressions)
}

fn solve_part_1(input) {
  let #(input_map, expressions) = input
  // Iterate over expressions and calculate the result which is always O(n^2) because I'm lazy,
  // but it'll work, I promise
  list.fold(expressions, input_map, fn(input_map, _expression) {
    list.fold(expressions, input_map, fn(input_map, expression) {
      let Expression(left, operator, right, new_var) = expression

      case dict.has_key(input_map, right) && dict.has_key(input_map, left) {
        True -> {
          let right_value = input_map |> dict.get(right) |> unpack()
          let left_value = input_map |> dict.get(left) |> unpack()

          let result = do_operation(operator, left_value, right_value)

          // I already hate these cases, and I can't write a macro to have ifs yet 😭
          // https://github.com/gleam-lang/suggestions/issues/22
          case dict.has_key(input_map, new_var) {
            True -> input_map
            False -> dict.insert(input_map, new_var, result)
          }
        }
        False -> input_map
      }
    })
  })
  |> generate_z00_values()
  |> int.base_parse(2)
  |> result.unwrap(-1)
  |> int.to_string()
}

fn generate_z00_values(input_map) {
  generate_boolean(0, "", input_map)
}

fn generate_boolean(current: Int, string_so_far: String, input_map) {
  let current_string = "z" <> int_to_string(current)

  case dict.has_key(input_map, current_string) {
    True -> {
      let zero_or_one =
        input_map
        |> dict.get(current_string)
        |> unpack()
        |> bool_to_string()

      generate_boolean(current + 1, zero_or_one <> string_so_far, input_map)
    }
    False -> string_so_far
  }
}

fn solve_part_2(input: #(dict.Dict(String, Bool), List(Expression))) {
  let #(_input_map, expressions) = input

  // I've allowed myself to look it in input.txt ;) I'm sorry
  let highest_z = "z45"

  list.fold(expressions, [], fn(wrong_list, expression) {
    let Expression(left, operator, right, new_var) = expression

    let wrong_list = case
      string.starts_with(new_var, "z")
      && operator != XOR
      && new_var != highest_z
    {
      True -> [new_var, ..wrong_list]
      False -> wrong_list
    }

    let xyz = ["x", "y", "z"]

    // This part is crazy, if you want to read more on it, go check
    // https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder
    let wrong_list = case
      operator == XOR
      && list.contains(xyz, right |> string.first() |> unpack())
      && list.contains(xyz, left |> string.first() |> unpack())
      && list.contains(xyz, new_var |> string.first() |> unpack())
    {
      True -> [new_var, ..wrong_list]
      False -> wrong_list
    }

    let wrong_list = case operator == AND && left != "x00" && right != "x00" {
      True ->
        list.fold(expressions, wrong_list, fn(wrong_list, expression) {
          let Expression(sub_left, sub_operator, sub_right, _sub_new_var) =
            expression

          case
            { new_var == sub_left || new_var == sub_right }
            && sub_operator != OR
          {
            True -> [new_var, ..wrong_list]
            False -> wrong_list
          }
        })
      False -> wrong_list
    }

    let wrong_list = case operator == XOR {
      True ->
        list.fold(expressions, wrong_list, fn(wrong_list, expression) {
          let Expression(sub_left, sub_operator, sub_right, _sub_new_var) =
            expression

          case
            // This one was weird, so in gleam we group expressions with `{}` instead 
            // of `()`
            { new_var == sub_left || new_var == sub_right }
            && sub_operator == OR
          {
            True -> [new_var, ..wrong_list]
            False -> wrong_list
          }
        })
      False -> wrong_list
    }

    wrong_list
  })
  |> list.sort(by: string.compare)
  |> list.unique()
  |> string.join(",")
}

pub fn main() {
  let input = parse_input()
  io.print(solve_part_1(input) <> "\n")
  io.print(solve_part_2(input))
}
