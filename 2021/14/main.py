with open("input.txt") as f:
    lines = f.read().split("\n")
    start = lines[0]
    rules_ = lines[2:]
    rules = {}

    for rule in rules_:
        parts = rule.split(" -> ")
        rules[parts[0]] = parts[1]

    rules2 = {}
    for rule, v in rules.items():
        rules2[rule] = (rule[0] + v, v + rule[1])

    startd = {k: 0 for k in rules2.keys()}
    for i in range(len(start) - 1):
        k = startd[start[i] + start[i + 1]]
        startd.setdefault(k, 0)
        startd[start[i] + start[i + 1]] += 1

    counts = {v: 0 for v in rules.values()}
    for v in start:
        counts[v] += 1

    def step(poly, cs):
        newpoly = {}
        for k, v in poly.items():
            if v == 0:
                continue
            (a, b) = rules2[k]
            newpoly.setdefault(a, 0)
            newpoly.setdefault(b, 0)
            newpoly[a] += v
            newpoly[b] += v

            al = rules[k]
            cs[al] += v
        return newpoly, cs

    for x in range(40):
        startd, counts = step(startd, counts)

    print(max(counts.values()) - min(counts.values()))
