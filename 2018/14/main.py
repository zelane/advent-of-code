i = 509671

def thing(recipies, a, b):
    x = recipies[a] + recipies[b]
    recipies += [int(y) for y in str(x)]
    a = (a + recipies[a] + 1) % len(recipies)
    b = (b + recipies[b] + 1) % len(recipies)
    return a, b

recipies = [3, 7]
a, b = 0, 1
si = str(i)
lsi = len(si)

done1 = done2 = False
while not done1 or not done2:
    a, b = thing(recipies, a, b)
    if not done1 and len(recipies) > i + 10:
        answer_1 = "".join(str(x) for x in recipies[i:i+10])
        done1 = True
    
    r = "".join([str(x) for x in recipies[-lsi-1:]])
    if not done2 and si in r:
        answer_2 = len(recipies) - lsi - (1 if r.startswith(si) else 0)
        done2 = True

print(answer_1)
print(answer_2)
