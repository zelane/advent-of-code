import re
from collections import defaultdict

with open('input.txt') as f:
    lines = f.readlines()
    initial = lines[0].split(": ")[1]
    rules = dict(
        tuple(re.findall("(.*) => (.*)", line)[0]) for line in lines[2:]
    )

def generate(pots):
    news_pots = defaultdict(lambda : ".")
    for i in range(min(pots.keys()) -2, max(pots.keys()) + 2):
        pattern = "".join(pots[x] for x in range(i-2, i+3))
        new_state = rules.get(pattern, ".")
        news_pots[i] = new_state

    result = sum(k for k, v in news_pots.items() if v == "#")
    return result, news_pots


pots = defaultdict(lambda : ".")
for i, p in enumerate(initial):
    pots[i] = p

results = [0]
prev_diff = -1
gen = 0
while True:
    result, pots = generate(pots)
    gen += 1
    if gen == 20:
        print(result)

    diff = result - results[gen-1]
    if diff == prev_diff:
        answer_2 = result + ((50_000_000_000 - gen) * diff)
        print(answer_2)
        break

    prev_diff = diff
    results.append(result)
