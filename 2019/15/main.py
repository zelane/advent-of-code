from collections import defaultdict
from copy import copy
from operator import itemgetter

with open("input.txt") as f:
    prog = [int(x) for x in f.read().split(",")]

arg_counts = {1: 3, 2: 3, 3: 1, 4: 1, 5: 2, 6: 2, 7: 3, 8: 3, 9: 1, 99: 0}


class Cpu:
    def __init__(self, prog, default_inputs=None):
        self.i = 0
        self.rb = 0
        self.mem = defaultdict(int)
        for k, v in enumerate(prog):
            self.mem[k] = v
        self.inputs = default_inputs if default_inputs else []
        self.halted = False

    def next_instruction(self):
        ins = str(self.mem[self.i])
        self.i += 1
        opt = int(ins[-2:])
        arg_count = arg_counts[opt]
        modes = list(ins[:-2].zfill(arg_count))
        args = list(self.mem.values())[self.i:self.i + arg_count]
        self.i += arg_count
        return opt, args, modes

    def vals(self, args, modes, write=False):
        write_address = []
        if write:
            arg = args.pop()
            write_mode = int(modes[0])
            if write_mode == 0:
                write_address = [arg]
            elif write_mode == 2:
                write_address = [self.rb + arg]
            else:
                raise Exception("Unknown write mode", write_mode)

        vals = []
        for arg in args:
            mode = int(modes.pop())
            if mode == 0:
                vals.append(self.mem[arg])
            elif mode == 1:
                vals.append(arg)
            elif mode == 2:
                vals.append(self.mem[self.rb + arg])

        return tuple(vals + write_address)

    def run(self, inputs=[]):
        self.inputs += inputs
        outputs = []
        while not self.halted:
            opt, args, modes = self.next_instruction()
            if opt == 99:
                self.halted = True

            elif opt == 1:
                a, b, c = self.vals(args, modes, True)
                self.mem[c] = a + b

            elif opt == 2:
                a, b, c = self.vals(args, modes, True)
                self.mem[c] = a * b

            elif opt == 3:
                if self.inputs:
                    c, = self.vals(args, modes, True)
                    self.mem[c] = int(self.inputs[0])
                    self.inputs = self.inputs[1:]
                else:
                    self.i -= 2
                    return outputs

            elif opt == 4:
                outputs.append(self.vals(args, modes)[0])

            elif opt == 5:
                a, b = self.vals(args, modes)
                if a != 0:
                    self.i = b

            elif opt == 6:
                a, b = self.vals(args, modes)
                if a == 0:
                    self.i = b

            elif opt == 7:
                a, b, c = self.vals(args, modes, True)
                self.mem[c] = 1 if a < b else 0

            elif opt == 8:
                a, b, c = self.vals(args, modes, True)
                self.mem[c] = 1 if a == b else 0

            elif opt == 9:
                a, = self.vals(args, modes)
                self.rb += a

        return outputs


def render(pos, _map, route):
    min_x = min(_map.keys(), key=itemgetter(0))[0]
    max_x = max(_map.keys(), key=itemgetter(0))[0]
    min_y = min(_map.keys(), key=itemgetter(1))[1]
    max_y = max(_map.keys(), key=itemgetter(1))[1]

    for y in reversed(range(min_y, max_y + 1)):
        for x in reversed(range(min_x, max_x + 1)):
            c = _map.get((x, y), "?")
            if (x, y) == pos:
                c = "*"
            elif (x, y) in route:
                c = "."
            print(c, end="")
        print("")


trans = {1: (0, 1), 2: (0, -1), 3: (-1, 0), 4: (1, 0)}
cells = {0: "#", 1: " ", 2: "O"}

_map = {}
neigh = {(0, 0): {}}
route = [(0, 0)]
oxy_pos = None

pos = (0, 0)
cpu = Cpu(copy(prog))

while not cpu.halted:
    backtrack = False
    # Look at all possible transformations
    for d, t in trans.items():
        neigh_pos = (pos[0] + t[0], pos[1] + t[1])
        # Never go back
        if neigh_pos in route:
            continue
        # Try the first unexplored neighbour
        if d not in neigh[pos]:
            dire = d
            break
    # If we've explored everything we need to backtrack
    else:
        if pos == (0, 0):
            break
        backtrack = True
        last = route[-2]
        t = (-1 * (pos[0] - last[0]), -1 * (pos[1] - last[1]))
        dire = [k for k, v in trans.items() if v == t][0]
        route.pop()

    output = cpu.run(inputs=[dire])[0]

    cell = cells[output]
    t = trans[dire]
    cell_pos = (pos[0] + t[0], pos[1] + t[1])
    _map[cell_pos] = cell
    neigh[pos][dire] = cell

    if backtrack:
        pos = cell_pos

    elif output in (1, 2):
        pos = cell_pos
        route.append(cell_pos)
        if pos not in neigh:
            neigh[pos] = {}

        if output == 2:
            oxy_pos = pos
            print(len(route))

render((0, 0), _map, [])

import time

minutes = 0
while True:
    updates = {}
    for pos, c in _map.items():
        if c == 'O':
            for d, t in trans.items():
                neigh_pos = (pos[0] + t[0], pos[1] + t[1])
                if _map[neigh_pos] not in ("#", "O"):
                    updates[neigh_pos] = "O"

    if not updates:
        print(minutes)
        break

    _map.update(updates)
    minutes += 1
    render((0, 0), _map, [])
    time.sleep(0.1)
