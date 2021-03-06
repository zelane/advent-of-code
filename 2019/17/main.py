from collections import defaultdict
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


_map = {}
x = y = 0
for o in Cpu(prog).run():
    c = chr(o)
    if c == "\n":
        x = 0
        y += 1
    else:
        _map[x, y] = c
        x += 1

answer_1 = 0
for p, c in _map.items():
    if c == "#":
        adj = [(p[0] - 1, p[1]), (p[0] + 1, p[1]), (p[0], p[1] - 1), (p[0], p[1] + 1)]
        if all(_map.get(a, "") == "#" for a in adj):
            answer_1 += p[0] * p[1]
print(answer_1)

        
def render(outputs):
    for o in outputs:
        if o < 255:
            print(chr(o), end="")
        else:
            print(o)


funcs = [
    "A,A,B,C,B,C,B,C,B,A",
    "L,10,L,8,R,8,L,8,R,6",
    "R,6,R,8,R,8",
    "R,6,R,6,L,8,L,10",
    "n\n"
]

prog[0] = 2
cpu = Cpu(prog)
outputs = cpu.run(inputs=list(map(ord, "\n".join(funcs))))
render(outputs)
