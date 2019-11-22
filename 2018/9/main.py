import re
from collections import deque

with open('input.txt') as f:
    player_count, max_m = tuple(int(x) for x in re.findall(r"[0-9]+", f.read()))

def play(player_count, max_m):
    players = [0 for _ in range(player_count)]
    circle = deque([0])

    for m in range(1, max_m + 1):
        if m % 23 == 0:
            circle.rotate(7)
            players[m % player_count] += m + circle.pop()
            circle.rotate(-1)
            continue

        circle.rotate(-1)
        circle.append(m)
    return max(players)

print(play(player_count, max_m))
print(play(player_count, max_m * 100))
