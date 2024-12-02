#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_LINE_LENGTH 1000

bool is_safe_pattern(int *numbers, int size, bool can_tolerate_error);

bool run_without_number(int *numbers, int size, int index)
{
    int *new_numbers = (int *)malloc((size - 1) * sizeof(int));
    memcpy(new_numbers, numbers, index * sizeof(int));
    memcpy(new_numbers + index, numbers + index + 1, (size - index - 1) * sizeof(int));
    bool result = is_safe_pattern(new_numbers, size - 1, false);
    free(new_numbers);
    return result;
}

bool handle_tolerance(int *numbers, int size, bool can_tolerate_error, int index)
{
    if (can_tolerate_error)
    {
        // Run again, but without current number OR without the next number
        return run_without_number(numbers, size, index) || run_without_number(numbers, size, index + 1);
    }
    else
    {
        return false;
    }
}

bool is_safe_pattern(int *numbers, int size, bool can_tolerate_error)
{
    if (size == 1)
    {
        return true;
    }

    if (numbers[0] == numbers[1])
    {
        return handle_tolerance(numbers, size, can_tolerate_error, 0);
    }

    bool increasing = numbers[0] < numbers[size - 1];

    for (int i = 0; i < size - 1; i++)
    {
        int abs_diff = abs(numbers[i] - numbers[i + 1]);
        if (increasing && numbers[i] > numbers[i + 1])
        {
            return handle_tolerance(numbers, size, can_tolerate_error, i);
        }
        else if (!increasing && numbers[i] < numbers[i + 1])
        {
            return handle_tolerance(numbers, size, can_tolerate_error, i);
        }
        else if (!(abs_diff >= 1 && abs_diff <= 3))
        {
            return handle_tolerance(numbers, size, can_tolerate_error, i);
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

        // Parse numbers from the line
        char *token = strtok(line, " ");
        while (token != NULL)
        {
            numbers[count++] = atoi(token);
            token = strtok(NULL, " ");
        }

        // Check if the pattern is safe
        if (is_safe_pattern(numbers, count, true))
        {
            printf("Safe: ");
            safe_reports_count++;
        }
        else
        {
            printf("Unsafe: ");
        }

        // Print the numbers in the line
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
