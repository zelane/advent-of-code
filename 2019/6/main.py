with open("test.txt") as f:
    _input = [x.strip().split(")") for x in f.readlines()]

_map = {}
for a, b in _input:
    for c, d in _input:
        _map.setdefault(c, set())
        if a == d:
            _map[c].add(a)
        if b == c:
            _map[c].add(d)


def count_orbits(planet, _map, direct):
    orbiting = _map.get(planet, [])
    indirect = 0
    for orbiting_planet in orbiting:
        indirect += count_orbits(orbiting_planet, _map, direct + 1)

    return direct + indirect


def find_path(current, route, _map):
    oribiting = _map.get(current, set())
    orbits = {p for p, orbits in _map.items() if current in orbits}
    if "SAN" in oribiting:
        print(len(route) - 1)

    options = (orbits | oribiting).difference(set(route))
    for _next in options:
        find_path(_next, route + [_next], _map)


print(count_orbits('COM', _map, 0))
find_path('YOU', [], _map)
