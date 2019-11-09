import re


with open('test.txt') as f:
    steps = {
        tuple(re.findall(r" ([A-Z]) ", line)) for line in f.readlines()
    }

def process(steps, workers, time):
    result = ""
    workers = [("", 0) for x in range(workers)]
    while True:
        pre_reqs = {step[1] for step in steps}
        to_do = sorted({s[0] for s in steps if s[0] not in pre_reqs})

        done = []
        for i, (job, time) in enumerate(workers):
            if time == 0:
                done.append(job)
                if to_do:
                    do = to_do.pop(index=0)
                    workers[i] = (do, 60)
            else:
                time -= 1

        steps = {step for step in steps if step[0] != done}
        if len(steps) == 0:
            return result + pre_reqs.pop()

answer_1 = process(steps, 1, 0)
print(answer_1)
# answer_2 = process(steps, 2, 0)

# Double sort?
