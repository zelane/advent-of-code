import re

rules = {}

with open("input.txt") as f:
    splits = f.read().split("\n\n")

    for line in splits[0].split("\n"):
        parts = line.split(": ")
        rules[parts[0]] = parts[1].replace('"', "")

    vals = [line for line in splits[1].split("\n")]


def parse(x, two=False):
    if x in ("a", "b"):
        return x
    elif "|" in x:
        return parse_or(x)
    return parse_r(x, two=two)


def parse_r(r, two=False):
    reg = ""
    for k in r.split(" "):
        if k == "8" and two:
            ft = parse("42")
            reg += f"{ft}+"
        elif k == "11" and two:
            ft = parse("42")
            to = parse("31")
            reg += (
                f"({ft}{to}|{ft}{{2}}{to}{{2}}|{ft}{{3}}{to}{{3}}|{ft}{{4}}{to}{{4}})"
            )
        else:
            x = rules[k]
            reg += parse(x)

    return reg


def parse_or(r):
    splits = r.split(" | ")
    left = parse(splits[0])
    right = parse(splits[1])
    return f"({left}|{right})"


regex = f"^{parse(rules['0'])}$"
regex2 = f"^{parse(rules['0'], two=True)}$"

answer1 = 0
answer2 = 0
for val in vals:
    if re.match(regex, val) is not None:
        answer1 += 1
    if re.match(regex2, val) is not None:
        answer2 += 1
print(answer1)
print(answer2)
