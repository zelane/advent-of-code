with open('input.txt') as f:
    polymer = f.read()

def react(_polymer):
    new_polymer = []
    for unit in _polymer:
        if new_polymer != [] and unit.swapcase() == new_polymer[-1]:
            new_polymer.pop()
            continue
        new_polymer.append(unit)

    return len(new_polymer)

answer_1 = react(polymer)
print(answer_1)

answer_2 = answer_1
for unit in set(polymer.lower()):
    test_polymer = polymer.replace(unit, "").replace(unit.upper(), "")

    length = react(test_polymer)
    if length < answer_2:
        answer_2 = length

print(answer_2)
