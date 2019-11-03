from collections import Counter

squares = {}
claims = {}

with open('input.txt') as f:
    for line in f.readlines():
        _id, _claim = line.split("@")
        offset, area = _claim.split(":")
        offset = [int(x) for x in offset.split(",")]
        area = [int(x) for x in area.split("x")]
        
        claim = set()
        for x in range(offset[0], offset[0] + area[0]):
            for y in range(offset[1], offset[1] + area[1]):
                square = (x, y)
                claim.add(square)
                if square not in squares:
                    squares[square] = 1
                else:
                    squares[square] += 1
        claims[_id] = claim

answer_1 = 0
for square, count in squares.items():
    if count > 1:
        answer_1 += 1

print(answer_1)

for _id, s in claims.items():
    for square in s:
        if squares.get(square) != 1:
            break
    else:
        print(_id)
        break
