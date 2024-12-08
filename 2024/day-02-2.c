#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

bool is_report_safe(const char *report)
{
    bool skip_allowed = true;
    const char *token = report;
    char *next_token;
    int previous_value;
    bool ascending = true;

    for (
        int value = strtol(token, &next_token, 10), i = 0;
        *token != '\0';
        value = strtol(token, &next_token, 10), ++i
    ) {
        // If we reach the end before we have enough elements to
        // process, ensure it isn't counted so that things like blank
        // lines are ignored. Otherwise, if we reach the end of the
        // line without finding anything unsafe, the report is safe.
        if (token == next_token) return i > 0;

        // Initialize values on first iteration.
        if (i == 0) {
            previous_value = value;
            token = next_token;
            value = strtol(token, &next_token, 10);
        }

        if (i == 1) {
            ascending = previous_value < value;
        }

        int difference = abs(value - previous_value);

        int unsafe = (
            difference < 1 || difference > 3 ||
            ascending && previous_value > value ||
            !ascending && previous_value < value
        );

        if (unsafe && skip_allowed) skip_allowed = false;
        else if (unsafe) return false;

        token = next_token;
        previous_value = value;
    }

    return true;
}

int count_safe_reports(const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(1);
    }

    int num_safe = 0;

    char *report = NULL;
    size_t len = 0;

    while (getline(&report, &len, file) != -1) {
        if (is_report_safe(report)) ++num_safe;
    }

    free(report);
    fclose(file);

    return num_safe;
}

int main(void)
{
    printf("%d\n", count_safe_reports("input.txt"));
}
