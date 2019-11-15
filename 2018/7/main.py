import re


with open('input.txt') as f:
    steps = {
        tuple(re.findall(r" ([A-Z]) ", line)) for line in f.readlines()
    }

def process(steps, workers, time):
    result = ""
    jobs = set()
    ttime = 0
    parts = {s[0] for s in steps}.union({s[1] for s in steps})
    while True:
        new_jobs = set()
        doing = set()
        for job, t in [x for x in jobs]:
            if t > 0:
                new_jobs.add((job, t-1))
                doing.add(job)
            else:
                parts.remove(job)
                result += job

        if len(parts) == 0:
            return ttime, result

        jobs = new_jobs

        pre_reqs = {step[1] for step in steps if step[0] not in result}
        to_do = sorted({p for p in parts if p not in pre_reqs and p not in doing})
        while len(jobs) < workers:
            if not to_do: break
            do = to_do.pop(0)
            jobs.add((do, (time + ord(do) - 65)))
        ttime += 1

answer_1 = process(steps, 1, 0)[1]
print(answer_1)
answer_2 = process(steps, 5, 60)[0]
print(answer_2)
