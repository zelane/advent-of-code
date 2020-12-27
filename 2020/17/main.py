cache = {}


def active_neighbours(coords, cubes, trans):
    result = 0
    neighbours = cache.get(coords)
    if not neighbours:
        neighbours = [tuple(map(sum, zip(tran, coords))) for tran in trans]
        cache[coords] = neighbours

    for n in neighbours:
        if cubes.get(n, ".") == "#":
            result += 1
    return result


def step(cubes, trans):
    new_cubes = {}
    for coords, cube in cubes.items():
        an = active_neighbours(coords, cubes, trans)
        if cube == "#":
            cube = "#" if an == 2 or an == 3 else "."
        elif cube == "." and an == 3:
            cube = "#"
        new_cubes[coords] = cube

    return new_cubes


def run(cubes, trans):
    for x in range(6):
        cubes = step(cubes, trans)

    return sum(1 for c in cubes.values() if c == "#")


r = list(range(-8, 12))
trange = list(range(-1, 2))

cubes3d = {(z, x, y): "." for x in r for y in r for z in r}
cubes4d = {(w, z, x, y): "." for w in r for z in r for x in r for y in r}

with open("input.txt") as f:
    for y, row in enumerate(f.read().split("\n")):
        for x, cube in enumerate(row):
            cubes3d[0, x, y] = cube
            cubes4d[0, 0, x, y] = cube

trans3d = [(z, x, y) for z in trange for x in trange for y in trange]
trans3d.remove((0, 0, 0))
print(run(cubes3d, trans3d))

trans4d = [(w, z, x, y) for w in trange for z in trange for x in trange for y in trange]
trans4d.remove((0, 0, 0, 0))
print(run(cubes4d, trans4d))
