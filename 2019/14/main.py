import re
import functools

reactions = {}
with open("test.txt") as f:
    for line in f.readlines():
        r = re.findall(r"[0-9]+ [a-zA-Z+]+", line)
        base_c, base_fid =  r.pop().split(" ")
        reactions[base_fid] = {
            'c': int(base_c),
            'ing': {}
        }
        for x in r:
            c, fid = x.split(" ")
            reactions[base_fid]['ing'][fid] = int(c)

def ore_count(fid, reactions, rem):
    ore = 0
    recp = reactions[fid]['ing']
    for sub_fid, sub_c in recp.items():
        if sub_fid == "ORE":
            return sub_c

        if sub_fid in rem:
            rem_c = rem[sub_fid]
            if sub_c >= rem_c:
                sub_c -= rem_c
                rem[sub_fid] = 0
            else:
                rem[sub_fid] -= sub_c
                continue

        makes = reactions[sub_fid]['c']
        while sub_c > 0:
            ore += ore_count(sub_fid, reactions, rem)
            sub_c -= makes
            if sub_c < 0:
                rem[sub_fid] += abs(sub_c)

    return ore

from collections import defaultdict
from copy import deepcopy

rem = {k: 0 for k in reactions.keys()}
fuel_r = reactions['FUEL']['ing']
answer_1 = ore_count('FUEL', reactions, rem)
print(answer_1)

ore = 1_000_000_000_000
fuel = 0
rem = {k: 0 for k in reactions.keys()}
while True:
    o = ore_count('FUEL', reactions, rem)
    if o > ore:
        break
    ore -= o
    print(fuel, ore, end='\r')
    fuel += 1

print(fuel, ore)
