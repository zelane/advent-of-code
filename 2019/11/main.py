from collections import defaultdict
from operator import itemgetter
from copy import copy

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


directions = {
    'N': ['W', 'E'],
    'E': ['N', 'S'],
    'S': ['E', 'W'],
    'W': ['S', 'N']
}

moves = {'N': (0, 1), 'E': (1, 0), 'S': (0, -1), 'W': (-1, 0)}


class Robot:
    def __init__(self, prog):
        self.pos = (0, 0)
        self.dir = 'N'
        self.cpu = Cpu(prog)

    def move(self, _dir: int):
        self.dir = directions[self.dir][_dir]
        move = moves[self.dir]
        self.pos = (self.pos[0] + move[0], self.pos[1] + move[1])


hull = defaultdict(int)
robot = Robot(copy(prog))

while not robot.cpu.halted:
    panel = hull[robot.pos]
    colour, dire = robot.cpu.run(inputs=[panel])
    hull[robot.pos] = colour
    robot.move(dire)
print(len(hull.keys()))

hull = defaultdict(int)
robot = Robot(copy(prog))
first = True
while not robot.cpu.halted:
    panel = hull[robot.pos]
    if first:
        panel = 1
        first = False

    outputs = robot.cpu.run(inputs=[panel])
    hull[robot.pos] = outputs[0]
    robot.move(outputs[1])

minx, maxx = min(hull, key=itemgetter(0))[0], max(hull, key=itemgetter(0))[0]
miny, maxy = min(hull, key=itemgetter(1))[1], max(hull, key=itemgetter(1))[1]

for y in reversed(range(miny, maxy + 1)):
    for x in range(minx, maxx):
        panel = hull.get((x, y))
        render = " "
        if panel is not None:
            render = "#" if int(panel) == 1 else " "
        print(render, end="")
    print("")
