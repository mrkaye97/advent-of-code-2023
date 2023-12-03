import copy

with open("data/day3.txt", "r") as f:
    data = f.read().split("\n")

numbers = [str(x) for x in range(10)]
line_length = len(data[0])

has_adjacent_symbol = {}

for row in range(len(data)):
    for column in range(len(data[row])):
        ul = data[row - 1][column - 1] if row > 0 and column > 0 else None
        u = data[row - 1][column] if row > 0 else None
        ur = data[row - 1][column + 1] if row > 0 and column < (len(data[row]) - 1) else None
        l = data[row][column - 1] if column > 0 else None
        r = data[row][column + 1] if column < (len(data[row]) - 1) else None
        dl = data[row + 1][column - 1] if (row < (len(data) - 1)) and column > 0 else None
        d = data[row + 1][column] if (row < (len(data) - 1)) else None
        dr = data[row + 1][column + 1] if (row < (len(data) - 1)) and column < (len(data[row]) - 1) else None


        result = [
            (ul and ul != "." and ul not in numbers),
            (u and u != "." and u not in numbers),
            (ur and ur != "." and ur not in numbers),
            (l and l != "." and l not in numbers),
            (r and r != "." and r not in numbers),
            (dl and dl != "." and dl not in numbers),
            (d and d != "." and d not in numbers),
            (dr and dr != "." and dr not in numbers)
        ]

        has_adjacent_symbol[str(row) + "," + str(column)] = any(result)


total = 0
for row_ix, row in enumerate(data):
    ix = 0
    while ix < len(row) - 1:
        char = row[ix]
        if char not in numbers:
            ix += 1
        else:
            inc = 0
            start = copy.deepcopy(ix)
            curr_char = ""
            char_block = curr_char
            while not curr_char or curr_char in numbers and start + inc < len(row):
                curr_char = row[start + inc]

                if not curr_char in numbers:
                    break

                char_block += curr_char
                inc += 1


            any_touches_sym = any([has_adjacent_symbol[str(row_ix) + "," + str(c)] for c in range(start, start + inc)])

            if any_touches_sym:
                total += int(char_block)

            ix += inc

print(total)