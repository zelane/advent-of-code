import re
from operator import itemgetter

with open('input.txt') as f:
    points = [
        tuple(map(int, tuple(re.findall(r"-?[0-9]+", line)))) for line in f.readlines()
    ]

def run(points):
    return [
        (x + xt, y + yt, xt, yt)
        for x, y, xt, yt in points
    ]

def render(points, min_y, max_y, min_x, max_x):
    for y in range(min_y, max_y):
        for x in range(min_x, max_x):
            for point in points:
                if (x, y) == point[0:2]:
                    print("#", end="")
                    break
            else:
                print(".", end="")
        print("\r")

answer_2 = 0
min_width = 9999999999
while True:
    prev_points = points
    points = run(points)

    min_x = min(points, key=itemgetter(0))[0]
    max_x = max(points, key=itemgetter(0))[0]
    min_y = min(points, key=itemgetter(1))[1]
    max_y = max(points, key=itemgetter(1))[1]

    width = max_x - min_x
    if width > min_width:
        render(prev_points, min_y, max_y, min_x, max_x)
        break
    min_width = width
    answer_2 += 1

print(answer_2)
