import 'dart:io';

List<int>? takeTwoInts(String line) {
  final RegExp regex = RegExp(r'-?\d+');
  final matches = regex.allMatches(line).map((m) => int.parse(m.group(0)!)).toList();
  if (matches.length != 2) {
    return null;
  }
  return matches;
}

List<List<int>> parseInput(String filePath) {
  final List<List<int>> coefficients = [];
  final List<int> group = [];

  final lines = File(filePath).readAsLinesSync();
  for (final line in lines) {
    final result = takeTwoInts(line);
    if (result != null) {
      group.addAll(result);
    } else if (group.isNotEmpty) {
      coefficients.add(List.from(group));
      group.clear();
    }
  }
  if (group.isNotEmpty) {
    coefficients.add(List.from(group));
  }

  return coefficients;
}

int solve(List<List<int>> listOfCoefficients, [String part = "first"]) {
  int totalCost = 0;

  for (var coeff in listOfCoefficients) {
    var k = coeff[0];
    var f = coeff[1];
    var d = coeff[2];
    var n = coeff[3];
    var z = coeff[4];
    var c = coeff[5];

    if (part == "second") {
      z += 10000000000000;
      c += 10000000000000;
    }

    // kx + dy = z
    // fx + ny = c
    // x = (dc - nz) / (df - kn)
    // y = (z - kx) / d
    // No solutions if (df - kn) == 0
    if ((d * f - k * n) == 0) {
      throw Exception(
          "Unlimited number of solutions since we're dealing with a line");
    }

    var x = (d * c - n * z) / (d * f - k * n);
    var y = (z - k * x) / d;

    if (x % 1 == 0 && y % 1 == 0) { // Check if x and y are integers
      totalCost += (x * 3 + y).toInt();
    }
  }

  return totalCost;
}

void main() {
  final filePath = "input.txt";
  final listOfCoefficients = parseInput(filePath);

  print("First part: ${solve(listOfCoefficients, "first")}");
  print("Second part: ${solve(listOfCoefficients, "second")}");
}