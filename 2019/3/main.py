with open("input.txt") as f:
    wire_a, wire_b = (l.split(",") for l in f.readlines())

directions = {
    'U': (0, 1),
    'D': (0, -1),
    'L': (-1, 0),
    'R': (1, 0),
}
port = (0, 0)


def calc_distance(a: tuple, b: tuple):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def trace_wire(path):
    steps = []
    x, y = port
    for step in path:
        dire = step[0]
        dist = int(step[1:])
        trans = directions[dire]
        for i in range(dist):
            x += trans[0]
            y += trans[1]
            steps.append((x, y))

    return steps


wire_a_steps = trace_wire(wire_a)
wire_b_steps = trace_wire(wire_b)

crosses = set(wire_a_steps).intersection(set(wire_b_steps))

answer_1 = min(calc_distance((0, 0), coord) for coord in crosses)
print(answer_1)

answer_2 = min(
    wire_a_steps.index(coord) + wire_b_steps.index(coord) + 2
    for coord in crosses)
print(answer_2)
