import re


with open('test.txt') as f:
    steps = {
        tuple(re.findall(r" ([A-Z]) ", line)) for line in f.readlines()
    }

def process(steps, workers, time):
    result = ""
    while True:
        pre_reqs = {step[1] for step in steps}
        done = sorted({s[0] for s in steps if s[0] not in pre_reqs})[0]
        result += done
        steps = {step for step in steps if step[0] != done}
        if len(steps) == 0:
            return result + pre_reqs.pop()

answer_1 = process(steps, 1, 0)
print(answer_1)
# answer_2 = process(steps, 2, 0)

# Double sort?
