pixels = open('input.txt').read()

w, h = 25, 6
i = 0
layers = []
counts = []
fewest_zeros = 9999
answer_1 = None

while i < len(pixels) - 1:
    layer = {}
    counts = [0, 0, 0]
    for y in range(h):
        for x in range(w):
            pixel = int(pixels[i])
            layer[(x, y)] = pixel
            counts[pixel] += 1
            i += 1

    if counts[0] < fewest_zeros:
        fewest_zeros = counts[0]
        answer_1 = counts[1] * counts[2]

    layers.append(layer)

print(answer_1)

spirtes = [" ", "#", " "]

for y in range(h):
    for x in range(w):
        pixel = (l[x, y] for l in layers)
        p = list(filter(lambda x: x != 2, pixel))[0]
        print(spirtes[p], end="")
    print("")
