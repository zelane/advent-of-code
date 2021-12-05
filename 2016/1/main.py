from collections import deque


def calculate_distance(instructions):
    location = (0, 0)
    transformations = deque(
        [(0, 1), (1, 0), (-1, 0), (0, -1)]  # North  # East  # South  # West
    )

    for instruction in instructions:
        dir = instruction[:1]
        dist = int(instruction[1:])

        if dir == "R":
            transformations.rotate(-1)
        elif dir == "L":
            transformations.rotate(1)

        vector = (transformations[0][0] * dist, transformations[0][1] * dist)
        location = (location[0] + vector[0], location[1] + vector[1])

    return abs(sum(location))


assert calculate_distance(("R2", "L3")) == 5
assert calculate_distance(("R2", "R2", "R2")) == 2
assert calculate_distance(("R5", "L5", "R5", "R3")) == 12

with open("input.txt") as f:
    instructions = f.read().split(", ")

print(calculate_distance(instructions))
