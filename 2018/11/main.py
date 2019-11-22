from operator import itemgetter
from collections import defaultdict


def calc_power(x, y, serial):
    rack_id = x + 10
    power = ((rack_id * y) + serial) * rack_id
    power = int(str(power)[-3:-2])
    return power - 5


def gen_summed_area(w, h, serial):
    grid = defaultdict(int)
    for x in range(w):
        for y in range(h):
            value = calc_power(x, y, serial)
            value += grid[x, y-1]  
            value += grid[x-1, y]  
            value -= grid[x-1, y-1]
            grid[x, y] = value
    return grid


def find_max(grid, size):
    max_power = 0
    top_left = (0, 0)
    
    for x in range(300 - size + 1):
        for y in range(300 - size + 1):
            x0, x1, y0, y1 = x-1, x+size-1, y-1, y+size-1
            a = grid[x0, y0]
            b = grid[x1, y0]
            c = grid[x0, y1]
            d = grid[x1, y1]
            power = d + a - b - c

            if power > max_power:
                max_power = power
                top_left = (x, y)

    return top_left, max_power, size


grid = gen_summed_area(300, 300, 4151)

answer_1, _, _ = find_max(grid, 3)
print(answer_1)

answer_2 = max((find_max(grid, size) for size in range(300)), key=itemgetter(1))
print(answer_2[0], answer_2[2])
