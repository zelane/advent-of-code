ingredients = set()
allergens = {}
counts = {}

with open("input.txt") as f:
    for line in f.readlines():
        parts = line.replace(")", "").replace("contains", "").split("(")
        ing = set(parts[0].strip().split(" "))
        ingredients = ingredients.union(ing)

        alle = parts[1].strip().split(", ")

        for i in ing:
            if i not in counts:
                counts[i] = 0
            counts[i] += 1

        for a in alle:
            if a not in allergens:
                allergens[a] = ing
            else:
                allergens[a] = allergens[a].intersection(ing)

solved = set()
while not all(len(x) == 1 for x in allergens.values()):
    for a, ings in allergens.items():
        if len(ings) == 1:
            solved |= ings
        else:
            allergens[a] = allergens[a].difference(solved)

known_allergens = set()
for x in allergens.values():
    known_allergens |= x

answer1 = sum(counts[a] for a in ingredients if a not in known_allergens)
print(answer1)

allergens = {k: allergens[k] for k in sorted(allergens)}
answer2 = ",".join([list(a)[0] for a in allergens.values()])
print(answer2)
