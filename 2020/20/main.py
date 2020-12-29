import json
from math import sqrt

tiles = {}
matching_edges = {}
size = 0

with open("test.txt") as f:
    ts = f.read().split("\n\n")

    for t in ts:
        lines = t.split("\n")
        tid = int(lines[0][5:-1])
        tiles[tid] = lines[1:]
        matching_edges[tid] = set()
        size += 1

size = int(sqrt(size))


def find_edges(tile):
    t = tile[0]
    r = "".join([x[-1] for x in tile])
    b = tile[-1]
    l = "".join([x[0] for x in tile])
    return t, r, b, l


TRANS = [(0, -1), (1, 0), (0, 1), (-1, 0)]


def flip_v(tile):
    return tile[::-1]


def flip_h(tile):
    return [t[::-1] for t in tile]


def rotate_r(tile):
    last = tile.pop()
    rotated = [c for c in last]
    while tile:
        last = tile.pop()
        for i, c in enumerate(last):
            rotated[i] += c
    return rotated


for tid, tile in tiles.items():
    edges = find_edges(tile)
    flipped = [s[::-1] for s in edges]
    for tid2, tile2 in tiles.items():
        if tid == tid2:
            continue

        edges2 = find_edges(tile2)
        for e in edges2:
            if e in edges:
                matching_edges[tid].add((edges.index(e), tid2))
            elif e in flipped:
                matching_edges[tid].add((flipped.index(e), tid2))

answer1 = 1
corners = []
edges = []
mids = []
for tid, matches in matching_edges.items():
    if len(matches) == 2:
        answer1 *= tid
        corners.append(tid)
    elif len(matches) == 3:
        edges.append(tid)
    else:
        mids.append(tid)

print(answer1)


def printt(tile):
    for row in tile:
        print(row)
    print("")


def place_edges(lcorner, edges):
    pass


def fit_corner(cid, corner, x, y):
    print(cid)
    printt(corner)
    if (x, y) == (0, 0):
        (ta, tb) = (1, 2)  # R, B
    elif (x, y) == (size, 0):
        (ta, tb) = (2, 3)  # B, L
    elif (x, y) == (0, size):
        (ta, tb) = (0, 1)  # T, R
    elif (x, y) == (size, size):
        (ta, tb) = (3, 0)  # L, T

    matches = list(matching_edges[cid])
    a = int(matches[0][0])
    b = int(matches[1][0])
    print(a, b)

    if a > b:
        corner = flip_h(corner)
        (a, b) = (b, a)
        printt(corner)

    print(a, b, "->", ta, tb)

    while (a, b) != (ta, tb):
        corner = rotate_r(corner)
        (a, b) = ((a + 1) % 4, (b + 1) % 4)
        print(a, b)

    printt(corner)


for c in corners:
    print(matching_edges[c])