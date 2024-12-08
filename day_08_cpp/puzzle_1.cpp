// g++ -std=c++11 -o puzzle_1 puzzle_1.cpp && ./puzzle_1
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::vector<std::vector<char>> readFileToVector(const std::string &filename)
{
    std::ifstream fin(filename);
    if (!fin)
    {
        throw std::runtime_error("Error: Could not open " + filename);
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(fin, line))
    {
        lines.push_back(line);
    }

    fin.close();

    int rows = static_cast<int>(lines.size());
    int cols = 0;
    if (rows > 0)
    {
        cols = static_cast<int>(lines[0].size());
    }

    std::vector<std::vector<char>> grid(rows, std::vector<char>(cols));

    for (int i = 0; i < rows; ++i)
    {
        for (int j = 0; j < cols; ++j)
        {
            grid[i][j] = lines[i][j];
        }
    }

    return grid;
}

template <typename T>
void printGrid(const std::vector<std::vector<T>> &grid)
{
    for (const auto &row : grid)
    {
        for (const auto &elem : row)
        {
            std::cout << elem;
        }
        std::cout << "\n";
    }
}

template <>
void printGrid(const std::vector<std::vector<bool>> &grid)
{
    for (const auto &row : grid)
    {
        for (bool elem : row)
        {
            std::cout << (elem ? '1' : '0');
        }
        std::cout << "\n";
    }
}

void printPositions(const std::vector<std::pair<int, int>> &positions)
{
    for (const auto &pos : positions)
    {
        std::cout << "(" << pos.first << ", " << pos.second << ")\n";
    }
}

bool isInGrid(int rows, int cols, int row, int col)
{
    return row >= 0 && row < rows && col >= 0 && col < cols;
}

std::vector<std::pair<int, int>> findPositions(const std::vector<std::vector<char>> &grid, int rowsNumber, int colsNumber, char ch)
{
    std::vector<std::pair<int, int>> result;

    for (int i = 0; i < rowsNumber; ++i)
    {
        for (int j = 0; j < colsNumber; ++j)
        {
            if (grid[i][j] == ch)
            {
                result.push_back(std::make_pair(i, j));
            }
        }
    }
    return result;
}

int main()
{
    std::vector<std::vector<char>> grid = readFileToVector("input.txt");
    int rowsNumber = grid.size();
    int colsNumber = grid[0].size();

    // 1. Create an empty array of size rowsNumber x colsNumber -> RESULT
    // 2. Iterate over each 0-9 and a-zA-Z character, and:
    // Having char X, we:
    // 2a. Generate all pairs of this letter -> K*(K-1) / 2 -> N^4
    // 2b. Append all generated antinodes to RESULT -> O(2)
    // 3. Count the number of antinodes in RESULT array

    // Total complexity -> O(N^4) where N is max(rowsNumber, colsNumber)

    std::vector<std::vector<bool>> antinodes(rowsNumber, std::vector<bool>(colsNumber, false));

    std::string supportedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (const char ch : supportedChars)
    {
        std::vector<std::pair<int, int>> positions = findPositions(grid, rowsNumber, colsNumber, ch);
        for (int i = 0; i < positions.size(); ++i)
        {
            for (int j = i + 1; j < positions.size(); ++j)
            {
                int row1 = positions[i].first;
                int col1 = positions[i].second;
                int row2 = positions[j].first;
                int col2 = positions[j].second;

                int rowOffset = row1 - row2;
                int colOffset = col1 - col2;

                int oppositeRow = (-1) * rowOffset;
                int oppositeCol = (-1) * colOffset;

                int firstRow = row1 + rowOffset;
                int firstCol = col1 + colOffset;

                int secondRow = row2 + oppositeRow;
                int secondCol = col2 + oppositeCol;

                if (isInGrid(rowsNumber, colsNumber, firstRow, firstCol))
                {
                    antinodes[firstRow][firstCol] = true;
                }

                if (isInGrid(rowsNumber, colsNumber, secondRow, secondCol))
                {
                    antinodes[secondRow][secondCol] = true;
                }
            }
        }
    }

    int antinodeCount = 0;
    for (int i = 0; i < rowsNumber; ++i)
    {
        for (int j = 0; j < colsNumber; ++j)
        {
            if (antinodes[i][j])
            {
                antinodeCount++;
            }
        }
    }

    printGrid(antinodes);

    std::cout << "Antinode count: " << antinodeCount << "\n";

    return 0;
}
