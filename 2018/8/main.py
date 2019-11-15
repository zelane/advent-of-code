with open('input.txt') as f:
    _input = tuple(int(x) for x in f.read().split(" "))

tree = {
    'children': [],
    'metadata': None
}

def parse_branch(offset, parent):
    leaf = {'children': [], 'metadata': None}
    meta_sum = 0
    c = _input[offset]
    m = _input[offset+1]

    offset += 2
    for _child in range(c):
        offset, child_sum = parse_branch(offset, leaf)
        meta_sum += child_sum

    leaf['metadata'] = _input[offset:offset+m]
    meta_sum += sum(leaf['metadata'])
    offset += m
    parent['children'].append(leaf)
    return offset, meta_sum

_, answer_1 = parse_branch(0, tree)

print(answer_1)
