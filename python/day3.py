import copy
from typing import Optional, List, Tuple

with open("data/day3.txt", "r") as f:
    data = f.read().split("\n")

numbers = [str(x) for x in range(10)]
line_length = len(data[0])

has_adjacent_symbol = {}

def capture_number_from_starting_position(row: List[str], starting_position: int) -> Optional[Tuple[int, int, int]]:
    ## ..314...
    ## start: 4
    ## return: (3, 5, 314)
    ## in format (start, end, value)

    if row[starting_position] not in numbers:
        return None

    captured = ""
    left_index = copy.deepcopy(starting_position)
    while True:
        left_index -= 1
        if left_index < 0:
            break

        if row[left_index] not in numbers:
            break

        if row[left_index] in numbers:
            captured = row[left_index] + captured

    captured += row[starting_position]
    right_index = copy.deepcopy(starting_position)
    while True:
        right_index += 1
        if right_index >= len(row):
            break

        if row[right_index] not in numbers:
            break

        if row[right_index] in numbers:
            captured += row[right_index]

    return (left_index, right_index, captured)

def determine_rows_to_check(data: List[List[str]], current_row: int) -> List[int]:
    if current_row == 0:
        return [current_row, current_row + 1]
    elif current_row == len(data):
        return [current_row - 1, current_row]
    else:
        return [current_row - 1, current_row, current_row + 1]

def determine_cols_to_check(row: List[str], current_col: int) -> List[int]:
    if current_col == 0:
        return [current_col, current_col + 1]
    elif current_col == len(row) - 1:
        return [current_col - 1, current_col]
    else:
        return [current_col - 1, current_col, current_col + 1]



def find_adjacent_numbers(data: List[List[str]], row: int, column: int) -> List[int]:
    rows_to_check = determine_rows_to_check(data=data, current_row=row)
    cols_to_check = determine_cols_to_check(row=data[row], current_col=column)

    result = [
        capture_number_from_starting_position(data[r], c)
        for r in rows_to_check
        for c in cols_to_check
    ]

    return {
        r
        for r in result
        if r
    }



result = [
    find_adjacent_numbers(data=data, row=r, column=c)
    for r in range(len(data))
    for c in range(len(data[r]))
    if data[r][c] != "." and data[r][c] not in numbers
]

print(sum([int(record[2]) for r in result for record in r]))
