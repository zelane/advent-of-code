from datetime import datetime, time
from operator import itemgetter


with open('input.txt') as f:
    _input = tuple(line for line in f.readlines())
_input = sorted(_input)

guards = {}

guard_id = None
slept_at = None

for line in _input:
    event = line[19:]
    dt = datetime.strptime(line[1:17], "%Y-%m-%d %H:%M")
    if event[0] == "G": # Began shift
        guard_id = int(event.split(" ")[1].replace("#", ""))
        guards.setdefault(guard_id, {})

    elif event[0] == "f": # Fell asleep
        slept_at = dt # Always past midnight

    elif event[0] == "w": # Wakes up
        for minute in range(slept_at.minute, dt.minute):
            guards[guard_id].setdefault(minute, 0)
            guards[guard_id][minute] += 1

laziest_guard = max(
    guards.items(), key=lambda guard: sum(guard[1].values())
)
top_minute = max(laziest_guard[1].items(), key=itemgetter(1))
answer_1 = laziest_guard[0] * top_minute[0]
print(answer_1)

most_consistent_guard =  max(
    guards.items(), key=lambda guard: max(guard[1].values() or [0])
)
top_minute = max(most_consistent_guard[1].items(), key=itemgetter(1))

answer_2 = most_consistent_guard[0] * top_minute[0]
print(answer_2)
