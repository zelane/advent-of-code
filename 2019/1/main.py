from math import floor

with open("input.txt") as f:
    _input = [float(x) for x in f.readlines()]

def calc_fuel(x:float):
    return floor((x / 3)) - 2

answer_1 = sum(
    calc_fuel(x) for x in _input
)
print(answer_1)

answer_2 = 0
for fuel in _input:
    while True:
        fuel = calc_fuel(fuel)
        if fuel < 1: break
        answer_2 += fuel

print(answer_2)
