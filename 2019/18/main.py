from string import ascii_lowercase, ascii_uppercase

import heapq

pos = (0, 0)
walls = tuple()
keys = {}
doors = {}

with open('input.txt') as f:
    y = 0
    for line in f.readlines():
        x = 0
        for c in line.strip():
            if c == "#":
                walls = walls + ((x, y), )
            elif c == "@":
                pos = (x, y)
            elif c in ascii_lowercase:
                keys[(x, y)] = c
            elif c in ascii_uppercase:
                doors[(x, y)] = c
            x += 1
        y += 1


def adjacent(x, y):
    return set([(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])


def shortest_path(pos, key_pos, walls):
    visited = set(walls)
    paths = []
    queue = [(0, [pos])]
    while queue:
        distance, path = heapq.heappop(queue)
        current_cell = path[-1]

        if paths and distance > len(paths[0]):
            break

        if current_cell == key_pos:
            paths.append(path[1:])
            paths = sorted(paths, key=lambda x: len(x))
            continue
        
        if current_cell in visited:
            continue

        visited.add(current_cell)
        options = adjacent(*current_cell)
        options = options.difference(visited)
        for cell in options:
            heapq.heappush(queue, (distance + 1, path + [cell]))

    return paths[0]


paths = {}
for posa, keya in list(keys.items()) + [(pos, "@")]:
    for posb, keyb in keys.items():
        if keyb == keya:
            continue

        path = shortest_path(posa, posb, walls)
        paths.setdefault(keya, {})[keyb] = {
            "dist": len(path), 
            "doors": [doors[d] for d in path if d in doors]
        }


def shortest_path(key, remaining, paths, cache):
    cache_key = key + "".join(remaining)
    cached = cache.get(cache_key)
    if cached:
        return cached

    remaining = set(remaining)
    remaining.discard(key)
    distances = []

    if len(remaining) == 0:
        return 0

    for next_key in remaining:
        details = paths[key][next_key]
        if any(d.lower() in remaining for d in details['doors']):
            continue

        dist = details['dist'] + shortest_path(next_key, remaining, paths, cache)
        distances.append(dist)

    cache[cache_key] = min(distances)
    return min(distances)

cache = {}
answer_1 = shortest_path("@", keys.values(), paths, cache)
print(answer_1)
