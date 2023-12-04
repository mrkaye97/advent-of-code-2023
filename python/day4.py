from dataclasses import dataclass


@dataclass
class LotteryCard:
    id: int
    winning_numbers: set[int]
    my_numbers: set[int]

    @property
    def wins(self):
        return self.my_numbers.intersection(self.winning_numbers)

    @property
    def value(self):
        return 2 ** (len(self.wins) - 1) if self.wins else 0

    def __add__(self, other):
        return self.value + other

    def __radd__(self, other):
        return self + other


with open("data/day4.txt", "r") as f:
    data = f.read().split("\n")


def parse_card(s: str) -> LotteryCard:
    card_id, card_numbers = s.split(":")
    winning_numbers, my_numbers = card_numbers.strip().split(" | ")

    return LotteryCard(
        id=int(card_id.strip().split("Card ")[1].strip()),
        winning_numbers={
            int(n.strip()) for n in winning_numbers.strip().split(" ") if n
        },
        my_numbers={int(n.strip()) for n in my_numbers.strip().split(" ") if n},
    )


cards = list(map(parse_card, data))

print("Part I Solution:", sum(cards))


def count_cards_after_spawning(
    card: LotteryCard, deck: list[LotteryCard] = cards
) -> int:
    if not card.wins:
        return 1

    new_cards_won = deck[card.id : (card.id + len(card.wins))]

    return 1 + sum([count_cards_after_spawning(c) for c in new_cards_won])


print("Part II Solution:", sum([count_cards_after_spawning(c) for c in cards]))
