from collections import deque

with open("input.txt") as f:
    inp = [int(x) for x in f.read().strip()]

# inp = [1, 2, 3, 4, 5, 6, 7, 8]
# inp = [int(x) for x in "80871224585914546619083218645595"]


def int_list(x):
    return int("".join(map(str, x)))


def apply_phase(inp):
    out = []
    for i in range(len(inp)):
        rep = deque()
        for y in [0, 1, 0, -1]:
            for n in range(i + 1):
                rep.append(y)

        rep.rotate(-1)
        z = 0
        for x in inp:
            z += x * rep[0]
            rep.rotate(-1)

        z = int(str(z)[-1])
        out.append(z)

    return out


answer_1 = [] + inp
for phase in range(100):
    answer_1 = apply_phase(answer_1)
print(int_list(answer_1[0:8]))

# answer_2 = inp * 10000
# for phase in range(100):
#     print(phase)
#     answer_2 = apply_phase(answer_2)

# offset = int_list(answer_2[0:7])
# print(answer_2[offset:offset + 8])
