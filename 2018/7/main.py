import re


with open('input.txt') as f:
    steps = {
        tuple(re.findall(r" ([A-Z]) ", line)) for line in f.readlines()
    }

answer_1 = ""
while True:
    pre_reqs = {step[1] for step in steps}
    done = sorted(s[0] for s in steps if s[0] not in pre_reqs)[0]
    answer_1 += done
    steps = {step for step in steps if step[0] != done}
    if len(pre_reqs) == 1: break

print(answer_1 + pre_reqs.pop())

# Double sort?
