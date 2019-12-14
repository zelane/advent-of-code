import re
from copy import deepcopy

with open("test.txt") as f:
    moons = [(list(map(int, re.findall(r'-?[0-9]+', x))), [0, 0, 0])
             for x in f.readlines()]


def apply_gravity(moon, moons):
    new_moon = deepcopy(moon)
    for other_moon in moons:
        pos, vel = other_moon
        for i in range(3):
            if pos[i] > moon[0][i]:
                new_moon[1][i] += 1
            elif pos[i] < moon[0][i]:
                new_moon[1][i] -= 1
    return new_moon


def apply_velocity(moon):
    for i in range(3):
        moon[0][i] += moon[1][i]


def calc_energies(moons):
    return sum(
        sum(map(abs, moon[0])) * sum(map(abs, moon[1])) for moon in moons)


init = deepcopy(moons)
i = 0
done = False
prev = [set(), set(), set(), set()]

while not done:
    new_moons = []
    for moon in moons:
        new_moon = apply_gravity(moon, moons)
        new_moons.append(new_moon)

    moons = new_moons
    for moon in moons:
        apply_velocity(moon)

    i += 1
    if i == 1000:
        print(calc_energies(moons))

print(len(prev))
