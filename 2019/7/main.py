from itertools import permutations

with open("input.txt") as f:
    prog = [int(x) for x in f.read().split(",")]

arg_counts = {1: 3, 2: 3, 3: 1, 4: 1, 5: 2, 6: 2, 7: 3, 8: 3, 99: 0}


class Amp:
    def __init__(self, prog, default_inputs=[]):
        self.i = 0
        self.mem = prog
        self.inputs = default_inputs
        self.halted = False

    def next_instruction(self):
        ins = str(self.mem[self.i])
        self.i += 1
        opt = int(ins[-2:])
        arg_count = arg_counts[opt]
        modes = list(ins[:-2].zfill(arg_count))
        args = self.mem[self.i:self.i + arg_count]
        self.i += arg_count
        return opt, args, modes

    def vals(self, args, modes, write=False):
        write_address = [args.pop()] if write else []
        vals = []
        for arg in args:
            mode = int(modes.pop())
            vals.append(arg if mode == 1 else self.mem[arg])
        return tuple(vals + write_address)

    def run(self):
        output = None
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
                    self.mem[args[0]] = int(self.inputs[0])
                    self.inputs = self.inputs[1:]
                else:
                    self.i -= 2
                    return output

            elif opt == 4:
                output = self.vals(args, modes)[0]

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

        return output


max_output = 0
for perm in permutations([0, 1, 2, 3, 4]):
    last_output = 0
    for x in perm:
        amp = Amp(prog, [x, last_output])
        last_output = amp.run()
    max_output = max(last_output, max_output)
print(max_output)

max_output = 0
for perm in permutations([5, 6, 7, 8, 9]):
    amps = [Amp(prog, [p]) for p in perm]
    last_output = 0
    while all(not amp.halted for amp in amps):
        for amp in amps:
            amp.inputs.append(last_output)
            last_output = amp.run()
    max_output = max(last_output, max_output)
print(max_output)
