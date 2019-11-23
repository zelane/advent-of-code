from collections import deque

with open("input.txt") as f:
    state = [
        [x for x in row]
        for row in f.readlines()
    ]

carts = ["^", ">", "v", "<"]
rotations = {'L': 1, 'S': 0, 'R': -1}
turns = {
    '^': {"/": ">", "\\": "<"},
    "<": {"/": "v", "\\": "^"},
    "v": {"/": "<", "\\": ">"},
    ">": {"/": "^", "\\": "v"}
}
trans = {
    "^": (0, -1), "<": (-1, 0), "v": (0, 1), ">": (1, 0)
}

class Cart:
    def __init__(self, c, x, y):
        self.c = c
        self.x = x
        self.y = y
        self.rot = deque(["L", "S", "R"]) 

    def move(self):
        tran = trans[self.c]
        self.x += tran[0]
        self.y += tran[1]

    def turn(self, track):
        self.c = turns[self.c][track]

    def junction(self):
        rot = rotations[self.rot[0]]
        i = (carts.index(self.c) - rot) % 4
        self.c = carts[i]
        self.rot.rotate(-1)

    def __eq__(self, cart):
        return self is not cart and self.x == cart.x and self.y == cart.y


all_carts = []
for y, row in enumerate(state):
    for x, cell in enumerate(row):
        if cell in carts:
            all_carts.append(Cart(cell, x, y))


def tick(state, carts):
    carts = sorted(carts, key=lambda c: (c.y, c.x))
    crashes = []
    for cart in carts:
        cart.move()
        target = state[cart.y][cart.x]
        if target == "+":
            cart.junction()        
        elif target in ("/", "\\"):
            cart.turn(target)
        
        for c in carts:
            if c == cart:
                crashes += [cart, c]

    return crashes


first_crash = True
while True:
    crashes = tick(state, all_carts)
    for c in crashes:
        if first_crash:
            print("{},{}".format(c.x, c.y))
            first_crash = False
        all_carts.remove(c)

    if len(all_carts) == 1:
        last_cart = all_carts[0]
        print("{},{}".format(last_cart.x, last_cart.y))
        break
