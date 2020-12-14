import re

with open("test.txt") as f:
    bags = {}
    for line in f.readlines():
        key = re.findall(r"[a-z]+ [a-z]+", line)[0]
        bags[key] = {}

        subs = re.findall(r"([1-9]+) ([a-z]+ [a-z]+)", line)
        for count, sub in subs:
            bags[key][sub] = int(count)

print(bags)


def find(key, contained_in):
    for bag, contained in bags.items():
        if key in contained:
            contained_in.add(bag)
            find(bag, contained_in)


def find2(key):
    total = 1
    for bag_key, count in bags[key].items():
        total += count * find2(bag_key)

    return total


contained_in = set()
find("shiny gold", contained_in)
print(len(contained_in))

x = find2("shiny gold")
print(x - 1)
