from itertools import groupby
from dataclasses import dataclass
from copy import deepcopy

with open("data/day5.txt", "r") as f:
    data = f.read().split("\n")

# data = [
#     "seeds: 79 14 55 13",
#     "",
#     "seed-to-soil map:",
#     "50 98 2",
#     "52 50 48",
#     "",
#     "soil-to-fertilizer map:",
#     "0 15 37",
#     "37 52 2",
#     "39 0 15",
#     "",
#     "fertilizer-to-water map:",
#     "49 53 8",
#     "0 11 42",
#     "42 0 7",
#     "57 7 4",
#     "",
#     "water-to-light map:",
#     "88 18 7",
#     "18 25 70",
#     "",
#     "light-to-temperature map:",
#     "45 77 23",
#     "81 45 19",
#     "68 64 13",
#     "",
#     "temperature-to-humidity map:",
#     "0 69 1",
#     "1 0 69",
#     "",
#     "humidity-to-location map:",
#     "60 56 37",
#     "56 93 4",
# ]


@dataclass
class SeedMap:
    destination_range_start: int
    source_range_start: int
    range_length: int


def parse_input(d) -> dict[str, list[list[int]]]:
    seeds = [int(x.strip()) for x in d[0].split(": ")[1].split(" ")]

    result = [list(g) for k, g in groupby(d[1:], key=bool) if k]

    return {
        "seeds": seeds,
        "maps": {
            x[0][:-1]: [
                SeedMap(
                    destination_range_start=int(y.split(" ")[0]),
                    source_range_start=int(y.split(" ")[1]),
                    range_length=int(y.split(" ")[2]),
                )
                for y in x[1:]
            ]
            for x in result
        },
    }


parsed = parse_input(data)

def map_value(v: int, xs: list[SeedMap]) -> int:
    for m in xs:
        d = m.destination_range_start
        s = m.source_range_start
        l = m.range_length

        ## If the value is in the range established
        ## by the source start and range (i.e. end)
        if v >= s and v < s + l:
            ## then we want the corresponding
            return d + (v - s)

    return v


out = []
for seed in parsed["seeds"]:
    value = deepcopy(seed)
    for m in parsed["maps"].values():
        value = map_value(value, m)

    out.append(value)

print("Part I Solution:", min(out))

seed_pairs = list(zip(*(iter(parsed["seeds"]),) * 2))

print(sum([s[1] for s in seed_pairs]))

## Idea for part II:
## for each seed range, split it into the possible matching "inputs"
## in the first map
## i.e. if the first map has two ranges 0,40 and 41,99
## then we have two cases:
##  1. the range is completely contained in an existing map range, e.g. 4, 30
##     and we know all of 4,30 (input) will map to the same output, so we can just do it once
##  2. the range spans multiple map ranges e.g. 35, 45
##     so we need to split it into N sub-ranges, i.e. 35,40 and 41, 45 and then operate on each of those
##
## Then we do the same thing at each step