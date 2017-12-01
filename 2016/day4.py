import re
from itertools import groupby

def validate_room(code):
    name, sector_id, checksum = re.findall(r"(.*)-(.*\d)\[(.*)\]", code)[0]
    name = sorted(name.replace("-", ""))
    counts = [(key, len(list(group))) for key, group in groupby(name)]
    ordered = sorted(counts, key=lambda x: (-x[1], x[0]))
    if checksum == "".join(x[0] for x in ordered[:5]):
        return int(sector_id)

assert validate_room("aaaaa-bbb-z-y-x-123[abxyz]") is not None
assert validate_room("a-b-c-d-e-f-g-h-987[abcde]") is not None
assert validate_room("not-a-real-room-404[oarel]") is not None
assert validate_room("totally-real-room-200[decoy]") is None

total = 0
with open("day4.txt") as f:
    for code in f.readlines():
        sector_id = validate_room(code)
        if sector_id:
            total += sector_id

print(total)
