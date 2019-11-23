import re
from collections import defaultdict

initial = ""
rules = {}
with open('input.txt') as f:
    lines = f.readlines()
    initial = lines[0].split(": ")[1]
    rules = {
        tuple(re.findall("(.*) => (.*)", line)[0]) for line in lines[2:]
    }
    rules = {x[0]: x[1] for x in rules}

pots = defaultdict(lambda : ".")

for i, p in enumerate(initial):
    pots[i] = p

for gen in range(20):
    news_pots = defaultdict(lambda : ".")
    for i in range(min(pots.keys()) -2, max(pots.keys()) + 2):
        pattern = "".join(pots[x] for x in range(i-2, i+3))
        new_state = rules.get(pattern, ".")
        news_pots[i] = new_state

    pots = news_pots

print(sum(k for k, v in pots.items() if v == "#"))
