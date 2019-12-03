from copy import copy

with open("input.txt") as f:
    prog = [int(x) for x in f.read().split(",")]

def run(prog, noun, verb):
    mem = copy(prog)
    mem[1] = noun
    mem[2] = verb
    i = 0
    while True:
        optcode = mem[i]
        if optcode == 99:
            break

        a, b, c = mem[i+1:i+4]
        if optcode == 1:
            mem[c] = mem[a] + mem[b]
        elif optcode == 2:
            mem[c] = mem[a] * mem[b]
        i += 4

    return mem[0]

print(run(prog, 12, 2))

for x, y in [(x, y) for x in range(100) for y in range(100)]:
    result = run(prog, x, y)
    if result == 19690720:
        print(100 * x + y)
