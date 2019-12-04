from collections import Counter

_min, _max = tuple(int(x) for x in "246515-739105".split("-"))


def test_codes(_min, _max, test):
    valid = 0
    for x in range(_min, _max):
        str_x = str(x)
        counts = Counter(str_x)
        if not any(test(c) for c in counts.values()):
            continue

        prev = str_x[0]
        for digit in str_x[1:]:
            if int(digit) < int(prev):
                break
            prev = digit
        else:
            valid += 1
    return valid


print(test_codes(_min, _max, lambda x: x >= 2))
print(test_codes(_min, _max, lambda x: x == 2))
