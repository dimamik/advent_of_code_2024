package main

import (
	"bufio"
	"fmt"
	"os"
)

func findXMAS(grid [][]rune) []string {
	var results []string
	rows := len(grid)
	cols := len(grid[0])
	target := "XMAS"

	checkWord := func(x, y, dx, dy int) bool {
		for i := 0; i < len(target); i++ {
			nx, ny := x+i*dx, y+i*dy
			if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != rune(target[i]) {
				return false
			}
		}
		return true
	}

	directions := [][2]int{
		{0, 1},
		{1, 0},
		{1, 1},
		{-1, 1},
		{0, -1},
		{-1, 0},
		{-1, -1},
		{1, -1},
	}

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			for _, dir := range directions {
				dx, dy := dir[0], dir[1]
				if checkWord(i, j, dx, dy) {
					results = append(results, fmt.Sprintf("Found at (%d, %d) in direction (%d, %d)", i, j, dx, dy))
				}
			}
		}
	}

	return results
}

func readGridFromFile(filename string) ([][]rune, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		grid = append(grid, []rune(line))
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return grid, nil
}

func main() {
	filename := "input.txt"
	grid, err := readGridFromFile(filename)
	if err != nil {
		fmt.Printf("Error reading file %s: %v\n", filename, err)
		return
	}

	results := findXMAS(grid)
	totalResults := len(results)

	fmt.Printf("%d\n", totalResults)
}
