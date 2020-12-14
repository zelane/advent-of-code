with open("input.txt") as f:
    lines = f.readlines()

lines = sorted([int(x) for x in lines])

lines = [0] + lines + [(max(lines) + 3)]
one_diffs = 0
three_diffs = 0

for i, x in enumerate(lines):
    if i + 1 == len(lines):
        break

    diff = lines[i + 1] - x
    if diff == 3:
        three_diffs += 1
    elif diff == 1:
        one_diffs += 1

answer1 = one_diffs * three_diffs
print(answer1)

all_paths = {}


def route(point, points):
    if point in all_paths:
        return all_paths[point]

    paths = 0
    for n in range(1, 4):
        x = point - n
        if x == 0:
            paths += 1
        elif x not in points:
            continue
        else:
            paths += route(x, points)

    all_paths[point] = paths
    return paths


answer2 = route(lines[-1], lines)

print(answer2)
