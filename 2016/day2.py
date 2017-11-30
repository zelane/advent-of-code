lines = []

with open("code.txt") as f:
    lines = f.read().split("\n")

keypad = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

position = (1, 1)

code = ""

for line in lines:
    for direction in line:
        if direction == "L":
            position = (position[0], max(0, position[1] - 1))
        elif direction == "R":
            position = (position[0], min(2, position[1] + 1))
        elif direction == "U":
            position = (max(0, position[0] - 1), position[1])
        elif direction == "D":
            position = (min(2, position[0] + 1), position[1])

    code += str(keypad[position[0]][position[1]])

print code