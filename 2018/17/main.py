import re

clay = set()
with open("test.txt") as f:
    for line in f.readlines():
        d = line[0]
        d1, r1, r2 = tuple(map(int, re.findall(r"[0-9]+", line)))
        print(d1, r1, r2)
        if d == "x":
            clay = clay | {(d1, y) for y in range(r1, r2+1)}
        else:
            clay = clay | {(x, d1) for x in range(r1, r2+1)}

def render(clay):
    for y in range(0, 14):
        for x in range(494, 510):
            c = "."
            if (x, y) == (500, 0):
                c = "+"
            elif (x, y) in clay:
                c = "#"
            print(c, end="")
        print("")

render(clay)