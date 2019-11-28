from copy import copy
import re


class Cpu:
    def __init__(self, mem):
        self.mem = mem

    def addr(self, a, b, c):
        self.mem[c] = self.mem[a] + self.mem[b]

    def addi(self, a, b, c):
        self.mem[c] = self.mem[a] + b

    def mulr(self, a, b, c):
        self.mem[c] = self.mem[a] * self.mem[b]

    def muli(self, a, b, c):
        self.mem[c] = self.mem[a] * b

    def banr(self, a, b, c):
        self.mem[c] = self.mem[a] & self.mem[b]

    def bani(self, a, b, c):
        self.mem[c] = self.mem[a] & b

    def borr(self, a, b, c):
        self.mem[c] = self.mem[a] | self.mem[b]

    def bori(self, a, b, c):
        self.mem[c] = self.mem[a] | b

    def setr(self, a, b, c):
        self.mem[c] = copy(self.mem[a])

    def seti(self, a, b, c):
        self.mem[c] = a

    def gtir(self, a, b, c):
        self.mem[c] = 1 if a > self.mem[b] else 0

    def gtri(self, a, b, c):
        self.mem[c] = 1 if self.mem[a] > b else 0

    def gtrr(self, a, b, c):
        self.mem[c] = 1 if self.mem[a] > self.mem[b] else 0

    def eqir(self, a, b, c):
        self.mem[c] = 1 if a == self.mem[b] else 0

    def eqri(self, a, b, c):
        self.mem[c] = 1 if self.mem[a] == b else 0

    def eqrr(self, a, b, c):
        self.mem[c] = 1 if self.mem[a] == self.mem[b] else 0


opts = [
    "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr",
    "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"
]

inputs = []
program = []
parse_mem = lambda s: list(map(int, re.findall(r"[0-9]", s)))
parse_params = lambda s: list(map(int, s.split(" ")))
with open('input.txt') as f:
    while True:
        before, args, after, _ = (f.readline().strip() for x in range(4))
        if not before:
            break
        inputs.append(
            (parse_mem(before), parse_params(args), parse_mem(after)))

    while True:
        line = f.readline()
        if not line:
            break
        program.append(parse_params(line))


def test(opts):
    solved = {}
    more_than_3 = 0
    for before, args, after in inputs:
        possible = []
        for func_name in opts:
            cpu = Cpu(copy(before))
            getattr(cpu, func_name)(*args[1:])
            if cpu.mem == after:
                possible.append(func_name)

        if len(possible) > 2:
            more_than_3 += 1
        elif len(possible) == 1:
            solved[args[0]] = possible[0]

    return more_than_3, solved


answer_1, opt_map = test(opts)
print(answer_1)

while opts:
    _, solved = test(opts)
    opt_map.update(solved)
    for opt in solved.values():
        opts.remove(opt)

cpu = Cpu([0, 0, 0, 0])
for args in program:
    func_name = opt_map[args[0]]
    getattr(cpu, func_name)(*args[1:])

print(cpu.mem[0])
