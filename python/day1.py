with open("data/day1.txt", "r") as f:
    data = f.read().split("\n")

def find_digit(s: str) -> str:
    for char in s:
        if char in map(lambda x: str(x), range(10)):
            return char

    return ""

result = sum(
    map(
        lambda line: int(find_digit(line) + find_digit(line[::-1])),
        data
    )
)

print(result)