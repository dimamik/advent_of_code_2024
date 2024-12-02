#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_LINE_LENGTH 1000

bool is_safe_pattern(int *numbers, int size)
{
    if (size == 1)
    {
        return true;
    }

    if (numbers[0] == numbers[1])
    {
        return false;
    }

    bool increasing = numbers[0] < numbers[size - 1];

    for (int i = 0; i < size - 1; i++)
    {
        int abs_diff = abs(numbers[i] - numbers[i + 1]);
        if (increasing && numbers[i] > numbers[i + 1])
        {
            return false;
        }
        else if (!increasing && numbers[i] < numbers[i + 1])
        {
            return false;
        }
        else if (!(abs_diff >= 1 && abs_diff <= 3))
        {
            return false;
        }
    }
    return true;
}

int main()
{
    FILE *file = fopen("input.txt", "r");
    if (file == NULL)
    {
        printf("Error: Could not open input file.\n");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    int safe_reports_count = 0;

    printf("Results:\n");

    while (fgets(line, sizeof(line), file))
    {
        int numbers[MAX_LINE_LENGTH];
        int count = 0;

        char *token = strtok(line, " ");
        while (token != NULL)
        {
            numbers[count++] = atoi(token);
            token = strtok(NULL, " ");
        }

        if (is_safe_pattern(numbers, count))
        {
            printf("Safe: ");
            safe_reports_count++;
        }
        else
        {
            printf("Unsafe: ");
        }

        for (int i = 0; i < count; i++)
        {
            printf("%d ", numbers[i]);
        }
        printf("\n");
    }

    fclose(file);

    printf("\nTotal safe patterns: %d\n", safe_reports_count);
    return 0;
}
