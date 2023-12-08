from math import prod
from functools import reduce

with open("data/day6.txt", "r") as f:
    data = f.read().split("\n")

data = list(zip(*map(lambda x: x.split(":")[1].split(), data)))

def determine_num_winning_charge_times(race: tuple[str, str]) -> int:
    race_duration = int(race[0])
    race_record_distance = int(race[1])

    for charge_time in range(race_duration + 1):
        speed = charge_time
        distance_traveled = speed * (race_duration - charge_time)

        if distance_traveled > race_record_distance:
            return 1 + race_duration - 2 * charge_time

    return 0

print("Part I Solution:", prod(map(determine_num_winning_charge_times, data)))
print("Part II Solution:", determine_num_winning_charge_times(reduce(lambda l, r: (l[0] + r[0], l[1] + r[1]), data)))