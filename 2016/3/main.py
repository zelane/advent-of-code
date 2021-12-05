def validate_triangle(a, b, c):
    return all([(a + b > c), (b + c > a), (c + a > b)])


assert validate_triangle(5, 10, 25) == False

valid_triangles = 0
with open("input.txt") as f:
    for line in f.readlines():
        a, b, c = line.split()
        if validate_triangle(int(a), int(b), int(c)):
            valid_triangles += 1

print(valid_triangles)
