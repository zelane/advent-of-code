import math
from operator import itemgetter

asteroids = set()

with open("input.txt") as f:
    for y, line in enumerate(f.readlines()):
        for x, cell in enumerate(line.strip()):
            if cell == "#":
                asteroids.add((x, y))


def between(a, b, c):
    return dist_between(a, c) < dist_between(a, b)


def dist_between(a, b):
    return math.hypot(abs(a[0] - b[0]), abs(a[1] - b[1]))


some = {}
for a in asteroids:
    some[a] = 0
    angles = {(bx, by): math.atan2(by - a[1], bx - a[0])
              for bx, by in asteroids}
    for b in asteroids:
        if a == b:
            continue

        for c in asteroids.difference(set([a, b])):
            if angles[b] == angles[c] and between(a, b, c):
                break
        else:
            some[a] += 1

a, answer_1 = max(some.items(), key=itemgetter(1))
print(answer_1)

angles = {}
for bx, by in asteroids:
    ang = round(math.degrees(math.atan2(by - a[1], bx - a[0])), 1)
    angles.setdefault(ang, []).append((bx, by))

for degrees in angles:
    angles[degrees] = sorted(angles[degrees], key=lambda k: dist_between(a, k))

destroyed = 0
last_destroyed = None
angle = -90.0
while destroyed < 200:
    targets = angles.get(angle, [])
    if targets:
        last_destroyed = targets[0]
        angles[angle] = targets[1:]
        destroyed += 1

    angle = round(angle + 0.1, 1)
    if angle == 180.1:
        angle = -180.0

print(last_destroyed[0] * 100 + last_destroyed[1])
