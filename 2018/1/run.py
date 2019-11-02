from collections import deque

with open('src/input.txt') as f:
    input = [int(line) for line in f.readlines()]

print(sum(input))

input = deque(input)
previous_values = set()
current_value = 0

while True:
    current_value += input[0]
    
    if current_value in previous_values:
        print(current_value)
        break

    previous_values.add(current_value)
    input.rotate(-1)
