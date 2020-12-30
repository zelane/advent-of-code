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
    return t, r, b, l


def flip_v(tile):
    return tile[::-1]


def flip_h(tile):
    return [t[::-1] for t in tile]


def rotate_r(tile):
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
    flipped = [s[::-1] for s in edges]
    for tid2, tile2 in tiles.items():
        if tid == tid2:
            continue

        edges2 = find_edges(tile2)
        for e in edges2:
            if e in edges:
                matching_edges[tid].add(tid2)
            elif e in flipped:
                matching_edges[tid].add(tid2)

answer1 = 1
corners = []
for tid, matches in matching_edges.items():
    if len(matches) == 2:
        answer1 *= tid
        corners.append(tid)
print(answer1)


def printt(tile):
    for row in tile:
        print(row)
    print("")


def match_lr(r, tile):
    tile = copy(tile)
    for i in range(4):
        # print(r, tile)
        if "".join([x[0] for x in tile]) == r:
            return tile
        tile = rotate_r(tile)


def match_bt(b, tile):
    tile = copy(tile)
    for i in range(4):
        if tile[0] == b:
            return tile
        tile = rotate_r(tile)


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

    # tile = tiles[tid]
    # for i in range(4):
    #     if match := fit_right(tile, matches):
    #         return tile, match
    #     tile = rotate_r(tile)

    # tile = flip_h(tile)
    # for i in range(4):
    #     if match := fit_right(tile, matches):
    #         return tile, match
    #     tile = rotate_r(tile)


image = []
last = corners[0]

# input
tiles[last] = flip_h(tiles[last])
# test
# tiles[last] = rotate_r(tiles[last])
# tiles[last] = rotate_r(tiles[last])


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

# for row in image:
#     for i in range(10):
#         r = ""
#         for tile in row:
#             print(tile[i] + " ", end="")
#         print("")
#     print("")


def peel(tile):
    tile = tile[1:-1]
    return [t[1:-1] for t in tile]


peeled = [list(map(peel, row)) for row in image]

printt(peeled[0][0])

image = []
for row in peeled:
    for i in range(8):
        r = ""
        for tile in row:
            r += tile[i]
        image.append(r)


linel = len(image[0]) - 20
print(linel)
reg = (
    r"(..................#.).{%s}(#....##....##....###).{%s}(.#..#..#..#..#..#...)"
    % (linel, linel)
)


def scan(image):
    f = 0
    e = 0
    found = 0
    text = "".join(image)
    # x = re.findall(reg, text, overlapped=True)
    # print("aa", len(x))
    for i, line in enumerate(image):

        for match in re.findall("#....##....##....###", line, overlapped=True):
            start = line.index(match)
            end = start + len(match)
            e += 1
            if re.match("..................#.", image[i - 1][start:end]):
                f += 1
                if re.match(".#..#..#..#..#..#...", image[i + 1][start:end]):
                    found += 1
    return found


found = 0
for _ in range(4):
    found += scan(image)
    image = rotate_r(image)

image = flip_v(image)
for _ in range(4):
    found += scan(image)
    image = rotate_r(image)


hashes = 0
for row in image:
    hashes += row.count("#")
print(hashes - (15 * found))
