from enum import Enum
from collections import Counter

with open("data/day7.txt", "r") as f:
    data = f.read().split("\n")


class HandType(str, Enum):
    FIVE_OF_A_KIND = 7
    FOUR_OF_A_KIND = 6
    FULL_HOUSE = 5
    THREE_OF_A_KIND = 4
    TWO_PAIR = 3
    ONE_PAIR = 2
    HIGH_CARD = 1

card_values = {
    "A": 14,
    "K": 13,
    "Q": 12,
    "J": 11,
    "T": 10,
    "9": 9,
    "8": 8,
    "7": 7,
    "6": 6,
    "5": 5,
    "4": 4,
    "3": 3,
    "2": 2,
}

class Hand:
    def __init__(self, cards: str, bid: str):
        self.cards = [card_values[c] for c in list(cards)]
        self.bid = int(bid)

    @property
    def type(self) -> HandType:
        item_counts = Counter(self.cards)

        if len(set(self.cards)) == 1:
            return HandType.FIVE_OF_A_KIND
        elif any([v == 4 for v in item_counts.values()]):
            return HandType.FOUR_OF_A_KIND
        elif len(item_counts) == 2 and any([v == 3 for v in item_counts.values()]):
            return HandType.FULL_HOUSE
        elif any([v == 3 for v in item_counts.values()]):
            return HandType.THREE_OF_A_KIND
        elif sum([v == 2 for v in item_counts.values()]) == 2:
            return HandType.TWO_PAIR
        elif sum([v == 2 for v in item_counts.values()]) == 1:
            return HandType.ONE_PAIR
        else:
            return HandType.HIGH_CARD

    def __lt__(self, other):
        if self.type.value != other.type.value:
            return self.type.value < other.type.value
        else:
            for pair in zip(self.cards, other.cards):
                if pair[0] != pair[1]:
                    return pair[0] < pair[1]

    def __repr__(self):
        return f"<Hand Object> Cards: {self.cards}; Bid: {self.bid}; Type: {self.type}"

hands = list(map(lambda x: Hand(*x.split()), data))
print("Part I Solution:", sum([h.bid * (ix + 1) for ix, h in enumerate(sorted(hands))]))

