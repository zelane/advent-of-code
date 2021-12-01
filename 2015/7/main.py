from functools import lru_cache
import regex as re

exps = {}


with open("input.txt") as f:
    for l in f.readlines():
        if r := re.match("^([a-z0-9]+) -> ([a-z0-9]+)$", l):
            a, k = r.groups()
            exps[k] = (a,)
        elif r := re.match("^NOT ([a-z0-9]+) -> ([a-z0-9]+)$", l):
            a, k = r.groups()
            exps[k] = ("NOT", a)
        elif r := re.match("^([a-z0-9]+) ([A-Z]+) ([a-z0-9]+) -> ([a-z]+)$", l):
            a, f, b, k = r.groups()
            exps[k] = (f, a, b)


@lru_cache
def get_val(x):
    try:
        return int(x)
    except Exception:
        return exec(x)


def exec(k):
    ex = exps[k]
    if len(ex) == 1:
        return get_val(ex[0])
    elif len(ex) == 2:
        x = get_val(ex[1])
        b = "".join(["1" if b == "0" else "0" for b in "{0:016b}".format(x)])
        return int(b, 2)
    else:
        f, a, b = ex
        a = get_val(a)
        b = get_val(b)
        if f == "AND":
            return a & b
        elif f == "OR":
            return a | b
        elif f == "LSHIFT":
            return a << b
        elif f == "RSHIFT":
            return a >> b


answer1 = exec("a")
print(answer1)

get_val.cache_clear()

exps["b"] = (answer1,)
answer2 = exec("a")
print(answer2)
