from collections import Counter

with open('input.txt') as f:
    input = tuple(line for line in f.readlines())

twos = 0
threes = 0

for code in input:
    letter_counts = Counter(code)
    if any(x == 2 for x in letter_counts.values()):
        twos += 1
    if any(x == 3 for x in letter_counts.values()):
        threes += 1
    
print(twos * threes)

for codea in input:
    for codeb in input:
        matches = [
            letter for i, letter in enumerate(codea)
            if letter == codeb[i]
        ]
        if len(matches) == 26:
            print("".join(matches))
            exit(0)
