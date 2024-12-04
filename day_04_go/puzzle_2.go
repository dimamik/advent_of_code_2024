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
	patterns := []string{"MAS", "SAM"}

	for x := 1; x < rows-1; x++ {
		for y := 1; y < cols-1; y++ {
			if grid[x][y] != 'A' {
				continue
			}
			for _, diag1 := range patterns {
				for _, diag2 := range patterns {
					// Check first diagonal (from top-left to bottom-right)
					if grid[x-1][y-1] == rune(diag1[0]) &&
						grid[x][y] == rune(diag1[1]) &&
						grid[x+1][y+1] == rune(diag1[2]) &&
						// Check second diagonal (from top-right to bottom-left)
						grid[x-1][y+1] == rune(diag2[0]) &&
						grid[x][y] == rune(diag2[1]) &&
						grid[x+1][y-1] == rune(diag2[2]) {
						results = append(results, fmt.Sprintf("X-MAS found centered at (%d, %d)", x, y))
					}
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
