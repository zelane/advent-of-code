with open('input.txt') as f:
    _input = tuple(int(x) for x in f.read().split(" "))

tree = {'children': [], 'metadata': []}

def parse_branch(offset, parent):
    leaf = {'children': [], 'metadata': None, 'value': 0}
    value = 0
    meta_sum = 0
    c = _input[offset]
    m = _input[offset+1]

    offset += 2
    for _child in range(c):
        offset, child_sum, _ = parse_branch(offset, leaf)
        meta_sum += child_sum

    leaf['metadata'] = _input[offset:offset+m]
    if c == 0:
        value += sum(leaf['metadata'])
    else:
        for child_index in leaf['metadata']:
            try:
                value += leaf['children'][child_index-1]['value']
            except IndexError:
                pass

    leaf['value'] = value
    meta_sum += sum(leaf['metadata'])
    offset += m
    parent['children'].append(leaf)
    return offset, meta_sum, value

_, answer_1, answer_2 = parse_branch(0, tree)
print(answer_1, answer_2)
