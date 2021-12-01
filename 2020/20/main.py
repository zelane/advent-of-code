from math import sqrt
from copy import copy
import regex as re

tiles = {}
matching_edges = {}
size = 0

with open("input.txt") as f:
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
    return [t, r, b, l] + [s[::-1] for s in [t, r, b, l]]


def flip_v(tile):
    return tile[::-1]


def flip_h(tile):
    return [t[::-1] for t in tile]


def rotate(tile):
    tile = copy(tile)
    last = tile.pop()
    rotated = [c for c in last]
    while tile:
        last = tile.pop()
        for i, c in enumerate(last):
            rotated[i] += c
    return rotated


for tid, tile in tiles.items():
    edges = find_edges(tile)
    for tid2, tile2 in tiles.items():
        if tid == tid2:
            continue

        if any([e in edges for e in find_edges(tile2)]):
            matching_edges[tid].add(tid2)

answer1 = 1
corners = []
for tid, matches in matching_edges.items():
    if len(matches) == 2:
        answer1 *= tid
        corners.append(tid)
print(answer1)


def match_lr(r, tile):
    for i in range(4):
        if "".join([x[0] for x in tile]) == r:
            return tile
        tile = rotate(tile)


def match_bt(b, tile):
    for i in range(4):
        if tile[0] == b:
            return tile
        tile = rotate(tile)


def fit_right(last_tile, matches):
    r = "".join([x[-1] for x in last_tile])
    for id in matches:
        next_tile = tiles[id]
        if match := match_lr(r, next_tile):
            return id, match
        if match := match_lr(r, flip_h(next_tile)):
            return id, match


def fit_bottom(tile, matches):
    b = tile[-1]
    for id in matches:
        next_tile = tiles[id]
        if match := match_bt(b, next_tile):
            return id, match
        if match := match_bt(b, flip_h(next_tile)):
            return id, match


def fit(tid):
    matches = matching_edges[tid]
    # matches = [t for t in tiles.keys() if t != tid]

    tile = tiles[tid]
    match = fit_right(tile, matches)
    return tile, match


image = []
last = corners[0]

# input
tiles[last] = flip_h(tiles[last])


for y in range(size):
    l = last
    last_tile = tiles[last]
    row = [last_tile]

    for x in range(size - 1):
        a, (last, b) = fit(last)
        tiles[last] = b
        row.append(b)

    image.append(row)
    adj = matching_edges[l]

    bot = fit_bottom(last_tile, adj)
    if not bot:
        break
    last, match = bot
    tiles[last] = match


def peel(tile):
    tile = tile[1:-1]
    return [t[1:-1] for t in tile]


peeled = [list(map(peel, row)) for row in image]

image = []
for row in peeled:
    for i in range(8):
        r = ""
        for tile in row:
            r += tile[i]
        image.append(r)


linel = len(image[0]) - 20
reg = (
    r"(..................#.).{%s}(#....##....##....###).{%s}(.#..#..#..#..#..#...)"
    % (linel, linel)
)


def scan(image):
    text = "".join(image)
    found = len(re.findall(reg, text, overlapped=True))
    return found


found = 0
for _ in range(4):
    found += scan(image)
    image = rotate(image)

image = flip_v(image)
for _ in range(4):
    found += scan(image)
    image = rotate(image)


hashes = sum(row.count("#") for row in image)
print(hashes - (15 * found))
