import scala.io.Source

object RobotSimulation {
  type Position = (Int, Int)
  type Velocity = (Int, Int)
  type Robot = (Position, Velocity)

  def parseInput(filePath: String): List[Robot] = {
    val lines = Source.fromFile(filePath).getLines
    lines.map { line =>
      val parts = line.trim.split(" v=")
      val positionPart = parts(0).substring(2) // Remove "p="
      val velocityPart = parts(1)
      val Array(px, py) = positionPart.split(",").map(_.toInt)
      val Array(vx, vy) = velocityPart.split(",").map(_.toInt)
      ((px, py), (vx, vy))
    }.toList
  }

  def emulateNDays(nDays: Int, robotPositions: List[Robot], mapSize: (Int, Int)): List[Position] = {
    val (width, height) = mapSize
    robotPositions.map { case ((px, py), (vx, vy)) =>
      val newPx = (px + (vx * nDays)) % width
      val newPy = (py + (vy * nDays)) % height
      // Handle negative modulo by ensuring coordinates are always non-negative
      ((newPx + width) % width, (newPy + height) % height)
    }
  }

  def printMap(robotPositions: List[Position], mapSize: (Int, Int)): Unit = {
    val (width, height) = mapSize
    val positionSet = robotPositions.toSet
    for (y <- (height - 1) to 0 by -1) {
      for (x <- 0 until width) {
        if (positionSet.contains((x, y))) print("#") else print(".")
      }
      println()
    }
  }

  def countRobotsInQuadrants(positions: List[Position], mapSize: (Int, Int)): Int = {
    val (width, height) = mapSize
    val middleX = width / 2
    val middleY = height / 2
    val quadrantCounts = Array(0, 0, 0, 0) // [Q1, Q2, Q3, Q4]
    positions.foreach { case (x, y) =>
      if (x != middleX && y != middleY) {
        if (x > middleX && y < middleY) quadrantCounts(0) += 1 // Q1
        else if (x < middleX && y < middleY) quadrantCounts(1) += 1 // Q2
        else if (x < middleX && y > middleY) quadrantCounts(2) += 1 // Q3
        else quadrantCounts(3) += 1 // Q4
      }
    }
    quadrantCounts.product
  }

  def findGroups(positions: List[Position], mapSize: (Int, Int)): List[List[Position]] = {
    val (width, height) = mapSize
    val robotPositions = positions.toSet
    var visited = Set.empty[Position]
    var groups = List.empty[List[Position]]

    def dfs(start: Position): List[Position] = {
      var stack = List(start)
      var cluster = List.empty[Position]
      while (stack.nonEmpty) {
        val (x, y) = stack.head
        stack = stack.tail
        if (!visited.contains((x, y))) {
          visited += ((x, y))
          cluster ::= ((x, y))
          val neighbors = List(
            ((x - 1 + width) % width, y),    // Left (wrapped)
            ((x + 1) % width, y),           // Right (wrapped)
            (x, (y - 1 + height) % height), // Up (wrapped)
            (x, (y + 1) % height)           // Down (wrapped)
          )
          neighbors.foreach { neighbor =>
            if (robotPositions.contains(neighbor) && !visited.contains(neighbor)) {
              stack ::= neighbor
            }
          }
        }
      }
      cluster
    }

    positions.foreach { pos =>
      if (!visited.contains(pos)) {
        val group = dfs(pos)
        groups ::= group
      }
    }

    groups
  }

  def main(args: Array[String]): Unit = {
    // Parse input
    val robotPositions = parseInput("input.txt")
    val numRobots = robotPositions.length
    val mapSize = (101, 103)

    // Part 1
    val newPositions = emulateNDays(100, robotPositions, mapSize)
    printMap(newPositions, mapSize)
    println(s"Part 1: ${countRobotsInQuadrants(newPositions, mapSize)}")
    println("-" * 50)

    // Part 2
    val maxIterations = 10000
    var patternFound = false
    for (time <- 0 until maxIterations if !patternFound) {
      val positions = emulateNDays(time, robotPositions, mapSize)
      val groups = findGroups(positions, mapSize)
      val largestGroup = groups.maxBy(_.length)
      if (largestGroup.length >= 0.3 * numRobots) {
        println("Visualization of the Christmas tree pattern:")
        printMap(positions, mapSize)
        println(s"The fewest \"seconds\" required: $time")
        patternFound = true
      }
    }
  }
}