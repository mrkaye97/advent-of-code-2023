from itertools import groupby
from dataclasses import dataclass
from copy import deepcopy
from multiprocessing import cpu_count, Pool
import functools

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


def determine_location(seed: int, maps: dict[str, list[SeedMap]]) -> int:
    value = deepcopy(seed)
    for m in maps.values():
        value = map_value(value, m)

    return value

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

def determine_min_loc_for_seed_pair(pair: tuple[int, int], maps: dict[str, list[SeedMap]]) -> int:
    minimum = 99
    for s in range(pair[0], pair[0] + pair[1]):
        if s % 1_000_000 == 0:
            ## pair[0] is the start number
            ## pair[1] is the total
            ## s is the current value
            ## (s - pair[0]) / pair[1] is the pct done?
            print(pair, str(round(100 * (s - pair[0]) / pair[1], 3)) + "%")

        location = determine_location(s, maps)
        if location < minimum:
            minimum = location

    return minimum

def partition_seed_pair(pair: tuple[int, int]) -> list[tuple[int, int]]:
    minimum = pair[0]
    maximum = pair[0] + pair[1]
    chunk_size = 1_000_000
    num_chunks = pair[1] // chunk_size

    return [(minimum + c * chunk_size, minimum + (c + 1) * chunk_size - 1 if minimum + (c + 1) * chunk_size < maximum else maximum) for c in range(num_chunks + 1)]

if __name__ == "__main__":

    with open("data/day5.txt", "r") as f:
        data = f.read().split("\n")

    cores = cpu_count() - 2
    pool = Pool(cores)

    parsed = parse_input(data)

    curried = functools.partial(determine_location, maps=parsed["maps"])

    print("Part I Solution:", min(pool.map(curried, parsed["seeds"])))

    seed_pairs = list(zip(*(iter(parsed["seeds"]),) * 2))

    print(sum([s[1] for s in seed_pairs]))

    seed_pairs = [partition_seed_pair(p) for p in seed_pairs]
    seed_pairs = [item for sublist in seed_pairs for item in sublist]

    curried_2 = functools.partial(determine_min_loc_for_seed_pair, maps=parsed["maps"])
    print("Part II Solution:", min(pool.map(curried_2, seed_pairs)))
