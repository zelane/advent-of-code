from operator import itemgetter

coordinates = set()

with open('input.txt') as f:
    for coords in f.readlines():
        x, y = map(int, coords.split(", "))
        coordinates.add((x, y))

def calc_distance(a:tuple, b:tuple):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

min_x = min(coordinates, key=itemgetter(0))[0]
max_x = max(coordinates, key=itemgetter(0))[0]
min_y = min(coordinates, key=itemgetter(1))[1]
max_y = max(coordinates, key=itemgetter(1))[1]

infinite_coords = set([
    min(coordinates, key=lambda x: calc_distance(x, (min_x, min_y))),
    min(coordinates, key=lambda x: calc_distance(x, (min_x, max_y))),
    min(coordinates, key=lambda x: calc_distance(x, (max_x, min_y))),
    min(coordinates, key=lambda x: calc_distance(x, (max_x, max_y))),
])

closet_count = {c: [] for c in coordinates}
answer_2 = 0
for square in [(x, y) for x in range(min_x, max_x) for y in range(min_y, max_y)]:
    _sum = 0
    distances = {}
    for coord in coordinates:
        distance = calc_distance(square, coord)
        distances.setdefault(distance, set()).add(coord)
        _sum += distance

    if _sum < 10000:
        answer_2 += 1

    closest_coords = distances[min(distances.keys())]
    if len(closest_coords) == 1:
        closet_count[closest_coords.pop()].append(square)

answer_1 = max(
    len(points) for coord, points in closet_count.items()
    if coord not in infinite_coords
)
print(answer_1)
print(answer_2)
